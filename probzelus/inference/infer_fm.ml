type 'a guide = 'a Distribution.constraints

let guide d =
  match Distribution.constraints d with
  | Some c -> c
  | None -> failwith "Cannot create a guide from these constraints"

open Ztypes

include Infer_pf


let rec guide_size : type a. a guide -> int = function
  | Dirac _ -> 0
  | Bool -> 1
  | Real -> 2
  | Interval (_, _) -> guide_size Real
  | Left_bounded _ -> guide_size Real
  | Right_bounded _ -> guide_size Real
  | Pair (g1, g2) -> guide_size g1 + guide_size g2
  | List gs -> List.fold_left (fun acc g -> acc + guide_size g) 0 gs
  | Array gs -> Array.fold_left (fun acc g -> acc + guide_size g) 0 gs

let transform d f f_prim f_inv =
  let sample _ = f (Distribution.draw d) in
  let logpdf y =
    let x = f_inv y in
    Distribution.score (d, x) -. log (f_prim x)
  in
  Distribution.sampler (sample, logpdf)

let rec guide_dist :
  type a. a guide -> float array -> int -> a Distribution.t =
  function
  | Dirac x -> fun _ _ -> Distribution.dirac x
  | Bool -> assert false
  | Real ->
      fun thetas offset ->
        Distribution.normal (thetas.(offset), exp thetas.(offset + 1))
  | Interval (a, b) ->
      fun thetas offset ->
        transform
          (guide_dist Real thetas offset)
          (fun x -> a +. ((b -. a) /. (1. +. exp (-.x))))
          (fun x ->
            let exp_m_x = exp (-.x) in
            let one_plus_exp_m_x = 1. +. exp_m_x in
            (b -. a) *. exp_m_x /. (one_plus_exp_m_x *. one_plus_exp_m_x))
          (fun y -> -.log (((b -. a) /. (y -. a)) -. 1.))
  | Left_bounded a ->
      fun thetas offset ->
        transform
          (guide_dist Real thetas offset)
          (fun x -> a +. exp x)
          (fun x -> exp x)
          (fun y -> log (y -. a))
  | Right_bounded b ->
      fun thetas offset ->
        transform
          (guide_dist Real thetas offset)
          (fun x -> b -. exp (-.x))
          (fun x -> exp (-.x))
          (fun y -> -.log (b -. y))
  | Pair (g1, g2) ->
      fun thetas offset ->
        let size_g1 = guide_size g1 in
        let d1 = guide_dist g1 thetas offset in
        let d2 = guide_dist g2 thetas (offset + size_g1) in
        Distribution.of_pair (d1, d2)
  | List gs ->
      fun thetas offset ->
        let _, ds =
          List.fold_left_map
            (fun acc g ->
              let size_g = guide_size g in
              (acc + size_g, guide_dist g thetas acc))
            offset gs
        in
        Distribution.of_list ds
  | Array gs ->
      fun thetas offset ->
        let _, ds =
          List.fold_left_map
            (fun acc g ->
              let size_g = guide_size g in
              (acc + size_g, guide_dist g thetas acc))
            offset (Array.to_list gs)
        in
        Distribution.of_array (Array.of_list ds)

let guide_dist guide thetas = guide_dist guide thetas 0

let rec guide_logpdf :
  type a. a guide -> float array -> int -> a -> float array -> unit =
  function
  | Dirac _ -> fun _ _ _ _ -> ()
  | Bool -> assert false
  | Real ->
      fun thetas offset v output ->
        let v_minus_mu = v -. thetas.(offset) in
        let sigma2 = exp (2. *. thetas.(offset + 1)) in
        output.(offset) <- v_minus_mu /. sigma2;
        output.(offset + 1) <- (v_minus_mu *. v_minus_mu /. sigma2) -. 1.
  | Interval (a, b) ->
      fun thetas offset v output ->
        guide_logpdf Real thetas offset
          (-.log (((b -. a) /. (v -. a)) -. 1.)) output
  | Left_bounded a ->
      fun thetas offset v output ->
        guide_logpdf Real thetas offset (log (v -. a)) output
  | Right_bounded b ->
      fun thetas offset v output ->
        guide_logpdf Real thetas offset (-.log (b -. v)) output
  | Pair (g1, g2) ->
      fun thetas offset (v1, v2) output ->
        let size_g1 = guide_size g1 in
        guide_logpdf g1 thetas offset v1 output;
        guide_logpdf g2 thetas (offset + size_g1) v2 output
  | List gs ->
      fun thetas offset vs output ->
        let _ =
          List.fold_left2
            (fun sizes g v ->
               let size_g = guide_size g in
               guide_logpdf g thetas sizes v output;
               sizes + size_g)
            offset gs vs
        in
        ()
  | Array gs ->
      fun thetas offset vs output ->
        let _ =
          List.fold_left2
            (fun sizes g v ->
               let size_g = guide_size g in
               guide_logpdf g thetas sizes v output;
               sizes + size_g)
            offset (Array.to_list gs) (Array.to_list vs)
        in
        ()

let guide_logpdf guide thetas v =
  let output = Array.make (guide_size guide) 0. in
  guide_logpdf guide thetas 0 v output;
  output

module type UPDATE = sig
  type 'a guide
  type 'a t
  val to_guide : 'a Distribution.t -> 'a guide
  val to_distribution : 'a guide -> 'a t -> 'a Distribution.t
  val init : 'a guide -> 'a Distribution.t -> 'a t
  val update : 'a guide -> 'a t -> 'a Distribution.t -> ('a -> float) -> 'a t
end

type ('a, 'b) state = { state : 'a; mutable params : 'b option }

module Make(U : UPDATE) = struct
  let infer particles (Cnode { alloc; reset; step; copy }) =
    let alloc () = { state = alloc (); params = None } in
    let reset s = reset s.state; s.params <- None in
    let step s data = step s.state data in
    let copy src dst = copy src.state dst.state; dst.params <- src.params in

    let step s (prob, (params_prior, guide, data)) =
      let initial_score = prob.scores.(prob.idx) in
      (* 0. Get guide parameter from state *)
      let phi =
        match s.params with
        | Some phi -> phi
        | None -> U.init guide params_prior
      in

      (* 1. Build guide params distribution *)
      let params_dist = U.to_distribution guide phi in

      (* Helper function to execute one step of the model *)
      let model_step params =
        (* save context *)
        let work_state = alloc () in
        copy s work_state;
        (* execute one step *)
        prob.scores.(prob.idx) <- initial_score;
        let theta =
          match params with
          | None ->
              (* if no value are provided draw from prior *)
              Distribution.draw params_dist
          | Some params ->
              (* otherwise constrain on params_dist *)
              params
        in
        let output = step work_state (prob, (theta, data)) in
        (output, work_state, prob.scores.(prob.idx))
      in

      (* 2. Sample the next value, state, and score from the model *)
      let output, work_state, score = model_step None in

      (* 4. Reinforce params_dist using the model as a function of params *)
      let params_dist =
        U.update guide phi params_dist
          (fun params -> let _, _, score = model_step (Some params) in score)
      in

      (* 5. Restore the state *)
      copy work_state s;
      (* Add guide params phi in the state *)
      s.params <- Some params_dist;
      prob.scores.(prob.idx) <- score;
      let params_dist, _ =
        Distribution.split (U.to_distribution guide params_dist)
      in
      Distribution.of_pair (params_dist, Distribution.dirac output)
    in

    let Cnode { alloc; reset; step; copy } =
      infer particles (Cnode { alloc; reset; step; copy })
    in

    let step state (params_prior1, params_prior2, data) =
      let params_prior = Distribution.of_pair (params_prior1, params_prior2) in
      let guide = U.to_guide params_prior in
      Distribution.to_mixture (step state (params_prior, guide, data))
    in

    Cnode { alloc; reset; step; copy }
end
