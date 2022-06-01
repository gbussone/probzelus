type 'a guide = 'a Distribution.constraints

let guide d =
  match Distribution.constraints d with
  | Some c -> c
  | None -> failwith "Cannot create a guide from these constraints"

open Ztypes

include Infer_pf


let rec guide_size : type a. a guide -> int = function
  | Dirac _ -> 0
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

type apf_params = {
  apf_particles : int;
  apf_iter : int;
  apf_eta : float;
  apf_batch : int;
}

module type REINFORCE = sig
  val init : 'a guide -> 'a Distribution.t -> apf_params -> float array
  val reinforce :
    float array -> ('a -> float) -> 'a guide -> apf_params -> float array
end

module Sgd : REINFORCE = struct
  let rec gradient_desc thetas f params =
    if params.apf_iter = 0 then thetas
    else
      let grad_step =
        Array.init params.apf_batch (fun _ -> f thetas ())
        |> Owl.Mat.of_arrays |> Owl.Mat.sum ~axis:0
        |> fun g -> Owl.Mat.(g *$ (params.apf_eta /. float params.apf_batch))
      in
      let thetas =
        thetas
        |> (fun t -> Owl.Mat.of_array t 1 (Array.length t))
        |> Owl.Mat.(fun t -> t - grad_step)
        |> Owl.Mat.to_array
      in

    (* TODO: See if we need Owl, e.g., to scale with more than 2 params *)
    (* let thetas =
       Array.map2
         (* Approximate gradient with k samples *)
         (fun theta k_grad ->
           let grad = k_grad /. float k in
           theta -. (eta *. grad))
         thetas
         (Array.fold_left (Array.map2 ( +. ))
            (Array.map (fun _ -> 0.) thetas)
            (Array.init k (fun _ -> f thetas ())))
         in *)
      gradient_desc thetas f { params with apf_iter = params.apf_iter - 1 }

  let rec reinforce thetas logscore q params =
    try
      gradient_desc thetas
        (fun thetas () ->
          let vs = Distribution.draw (guide_dist q thetas) in
          let q_thetas_vs = Distribution.score (guide_dist q thetas, vs) in
          let d_q_thetas_vs = guide_logpdf q thetas vs in
          let logscore = logscore vs in
          Array.mapi
            (fun i _ -> d_q_thetas_vs.(i) *. (q_thetas_vs -. logscore)) thetas)
        params
    with _ ->
      reinforce thetas logscore q
        { params with apf_eta = params.apf_eta /. 2. }

  let init guide prior params =
    reinforce (Array.make (guide_size guide) 0.)
      (fun v -> Distribution.score (prior, v)) guide params
end

module Adagrad : REINFORCE = struct
  let rec adagrad thetas f params grads =
    if params.apf_iter = 0 then thetas
    else
      let grad =
        Array.init params.apf_batch (fun _ -> f thetas ())
        |> Owl.Mat.of_arrays |> Owl.Mat.sum ~axis:0
      in
      let grads = Owl.Mat.(grads + grad * grad) in
      let thetas =
        thetas
        |> (fun t -> Owl.Mat.of_array t 1 (Array.length t))
        |> Owl.Mat.(fun t -> t - grad / sqrt grads *$ params.apf_eta)
        |> Owl.Mat.to_array
      in
      adagrad thetas f { params with apf_iter = params.apf_iter - 1 } grads

  let reinforce thetas logscore q params =
    adagrad thetas
      (fun thetas () ->
        let vs = Distribution.draw (guide_dist q thetas) in
        let q_thetas_vs = Distribution.score (guide_dist q thetas, vs) in
        let d_q_thetas_vs = guide_logpdf q thetas vs in
        let logscore = logscore vs in
        Array.mapi (fun i _ -> d_q_thetas_vs.(i) *. (q_thetas_vs -. logscore))
          thetas)
      params (Owl.Mat.create 1 (Array.length thetas) 1e-8)

  let init guide prior params =
    reinforce (Array.make (guide_size guide) 0.)
      (fun v -> Distribution.score (prior, v)) guide params
end

module Moment_matching : REINFORCE = struct
  let rec moment_matching :
    type a. a guide -> a Distribution.t -> int -> float array -> unit =
    function
    | Dirac _ -> fun _ _ _ -> ()
    | Real ->
        fun d offset output ->
          let m, s = Distribution.stats_float d in
          output.(offset) <- m;
          output.(offset + 1) <- log s
    | Interval _ -> assert false
    | Left_bounded _ -> assert false
    | Right_bounded _ -> assert false
    | Pair (g1, g2) ->
        fun d offset output ->
          let size_g1 = guide_size g1 in
          let d1, d2 = Distribution.split d in
          moment_matching g1 d1 offset output;
          moment_matching g2 d2 (offset + size_g1) output
    | List gs ->
        fun d offset output ->
          let _ =
            List.fold_left2
              (fun sizes g d ->
                 let size_g = guide_size g in
                 moment_matching g d sizes output;
                 sizes + size_g)
              offset gs (Distribution.split_list d)
          in
          ()
    | Array gs ->
        fun d offset output ->
          let _ =
            List.fold_left2
              (fun sizes g d ->
                 let size_g = guide_size g in
                 moment_matching g d sizes output;
                 sizes + size_g)
              offset (Array.to_list gs)
              (Array.to_list (Distribution.split_array d))
          in
          ()

  let moment_matching guide dist =
    let output = Array.make (guide_size guide) 0. in
    moment_matching guide dist 0 output;
    output

  let init guide prior _params = moment_matching guide prior

  let reinforce thetas logscore q _params =
    let values =
      Array.init 1000 (fun _ -> Distribution.draw (guide_dist q thetas))
    in
    let logits = Array.map logscore values in
    let _, dist = Normalize.normalize_nohist values logits in
    moment_matching q dist
end

type 'a state = { state : 'a; mutable params : float array option }

module Make(R : REINFORCE) = struct
  let infer params (Cnode { alloc; reset; step; copy }) =
    let alloc () = { state = alloc (); params = None } in
    let reset s = reset s.state; s.params <- None in
    let step s data = step s.state data in
    let copy src dst = copy src.state dst.state; dst.params <- src.params in

    let step s (prob, (params_prior, data)) =
      let guide = guide params_prior in
      let initial_score = prob.scores.(prob.idx) in
      (* 0. Get guide parameter from state *)
      let phi =
        match s.params with
        | Some phi -> phi
        | None -> R.init guide params_prior params
      in

      (* 1. Build guide params distribution *)
      let params_dist = guide_dist guide phi in

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
              observe' (prob, (params_dist, params));
              params
        in
        let output = step work_state (prob, (theta, data)) in
        (output, work_state, prob.scores.(prob.idx))
      in

      (* 2. Sample the next value, state, and score from the model *)
      let output, work_state, score = model_step None in

      (* 4. Reinforce params_dist using the model as a function of params *)
      let params_dist =
        R.reinforce phi
          (fun params -> let _, _, score = model_step (Some params) in score)
          guide params
      in

      (* 5. Restore the state *)
      copy work_state s;
      (* Add guide params phi in the state *)
      s.params <- Some params_dist;
      prob.scores.(prob.idx) <- score;
      (output, params_dist)
    in

    let Cnode { alloc; reset; step; copy } =
      infer params.apf_particles (Cnode { alloc; reset; step; copy })
    in

    let step state (params_prior, data) =
      let guide = guide params_prior in
      let results_dist = step state (params_prior, data) in

      (* Extract results *)
      let outputs = Distribution.map (fun (o, _) -> o) results_dist in
      let mixture =
        Distribution.to_mixture
          (Distribution.map
             (fun (_, p) ->
                let d, _ = Distribution.split (guide_dist guide p) in
                d)
             results_dist)
      in
      Distribution.of_pair (mixture, outputs)
    in

    Cnode { alloc; reset; step; copy }
end
