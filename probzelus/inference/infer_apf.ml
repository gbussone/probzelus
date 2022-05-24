type 'a guide = 'a Distribution.constraints

let guide d =
  match Distribution.constraints d with
  | Some c -> c
  | None -> failwith "Cannot create a guide from these constraints"

open Ztypes

type prob = { id : int; logits : float array }

let sample' (_prob, d) = Distribution.draw d
let sample =
  let alloc () = () in
  let reset _state = () in
  let copy _src _dst = () in
  let step _state input =
    sample' input
  in
  Cnode { alloc; reset; copy; step; }


let factor' (prob, s) = prob.logits.(prob.id) <- prob.logits.(prob.id) +. s
let factor =
  let alloc () = () in
  let reset _state = () in
  let copy _src _dst = () in
  let step _state input =
    factor' input
  in
  Cnode { alloc; reset; copy; step; }

let observe' (prob, (d, x)) = factor' (prob, Distribution.score (d, x))
let observe =
  let alloc () = () in
  let reset _state = () in
  let copy _src _dst = () in
  let step _state input =
    observe' input
  in
  Cnode { alloc; reset; copy; step; }


type 'a infer_state = { mutable particles : 'a array; scores : float array }


let rec guide_size : type a. a guide -> int = function
  | Dirac _ -> 0
  | Real -> 2
  | Interval (_, _) -> guide_size Real
  | Left_bounded _ -> guide_size Real
  | Right_bounded _ -> guide_size Real
  | Pair (g1, g2) -> guide_size g1 + guide_size g2
  | List gs -> List.fold_left (fun acc g -> acc + guide_size g) 0 gs

let transform d f f_prim f_inv =
  let sample _ = f (Distribution.draw d) in
  let logpdf y = let x = f_inv y in Distribution.score (d, x) -. log (f_prim x) in
  Distribution.sampler (sample, logpdf)

let rec guide_dist : type a. a guide -> float array -> int -> a Distribution.t = function
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

let guide_dist guide thetas = guide_dist guide thetas 0

let rec guide_logpdf : type a. a guide -> float array -> int -> a -> float array -> unit =
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

let guide_logpdf guide thetas v =
  let output = Array.make (guide_size guide) 0. in
  guide_logpdf guide thetas 0 v output;
  output

let rec gradient_desc thetas f eta k n =
  if n = 0 then thetas
  else
    let grad_step =
      Array.init k (fun _ -> f thetas ())
      |> Owl.Mat.of_arrays |> Owl.Mat.sum ~axis:0
      |> fun g -> Owl.Mat.(g *$ (eta /. float k))
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
    gradient_desc thetas f eta k (n - 1)

let rec adagrad thetas f eta k n grads =
  if n = 0 then thetas
  else
    let grad =
      Array.init k (fun _ -> f thetas ())
      |> Owl.Mat.of_arrays |> Owl.Mat.sum ~axis:0
    in
    let grads = Owl.Mat.(grads + grad * grad) in
    let thetas =
      thetas
      |> (fun t -> Owl.Mat.of_array t 1 (Array.length t))
      |> Owl.Mat.(fun t -> t - grad / sqrt grads *$ eta)
      |> Owl.Mat.to_array
    in
    adagrad thetas f eta k (n - 1) grads

let rec reinforce thetas logscore q eta k n =
  try
    gradient_desc thetas
      (fun thetas () ->
        let vs = Distribution.draw (guide_dist q thetas) in
        let q_thetas_vs = Distribution.score (guide_dist q thetas, vs) in
        let d_q_thetas_vs = guide_logpdf q thetas vs in
        let logscore = logscore vs in
        Array.mapi
          (fun i _ -> d_q_thetas_vs.(i) *. (q_thetas_vs -. logscore))
          thetas)
      eta k n
  with _ -> reinforce thetas logscore q (eta /. 2.) k n

let reinforce_adagrad thetas logscore q eta k n =
  adagrad thetas
    (fun thetas () ->
      let vs = Distribution.draw (guide_dist q thetas) in
      let q_thetas_vs = Distribution.score (guide_dist q thetas, vs) in
      let d_q_thetas_vs = guide_logpdf q thetas vs in
      let logscore = logscore vs in
      Array.mapi
        (fun i _ -> d_q_thetas_vs.(i) *. (q_thetas_vs -. logscore))
        thetas)
    eta k n (Owl.Mat.create 1 (Array.length thetas) 1e-8)

let support ~values ~logits =
  let _, d = Normalize.normalize_nohist values logits in
  d

let reinforce_match thetas logscore q _eta _k _n =
  let values =
    Array.init 1000 (fun _ -> Distribution.draw (guide_dist q thetas))
  in
  let logits = Array.map logscore values in
  let dist = support ~values ~logits in
  let m, s = Distribution.stats_float dist in
  [| m; log s |]

type apf_params = {
  apf_particles : int;
  apf_iter : int;
  apf_eta : float;
  apf_batch : int;
}

type 'a state = { state : 'a; mutable params : float array option }

let infer params (Cnode { alloc; reset; step; copy }) =
  let alloc () = { state = alloc (); params = None } in
  let reset s = reset s.state; s.params <- None in
  let step s data = step s.state data in
  let copy src dst = copy src.state dst.state; dst.params <- src.params in

  let nb_particles = params.apf_particles in
  let eta = params.apf_eta in
  let batch = params.apf_batch in
  let iter = params.apf_iter in

  let infer_alloc () =
    {
      particles = Array.init nb_particles (fun _ -> alloc ());
      scores = Array.make nb_particles 0.;
    }
  in
  let infer_reset state =
    Array.iter reset state.particles;
    Array.iteri (fun i _ -> state.scores.(i) <- 0.) state.scores
  in

  let infer_step state (params_prior, data) =
    let guide = guide params_prior in
    let particle_step prob s =
      let initial_score = prob.logits.(prob.id) in
      (* 0. Get guide parameter from state *)
      let phi =
        match s.params with
        | Some phi -> phi
        | None ->
            reinforce_adagrad
              (Array.make (guide_size guide) 0.)
              (fun v -> Distribution.score (params_prior, v))
              guide eta batch iter
      in

      (* 1. Build guide params distribution *)
      let params_dist = guide_dist guide phi in

      (* Helper function to execute one step of the model *)
      let model_step params =
        (* save context *)
        let work_state = alloc () in
        copy s work_state;
        (* execute one step *)
        prob.logits.(prob.id) <- initial_score;
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
        (output, work_state, prob.logits.(prob.id))
      in

      (* 2. Sample the next value, state, and score from the model *)
      let output, work_state, score = model_step None in

      (* 4. Reinforce params_dist using the model as a function of params *)
      let params_dist =
        reinforce_adagrad phi
          (fun params ->
            let _, _, score = model_step (Some params) in
            score)
          guide eta batch iter
      in

      (* 5. Restore the state *)
      copy work_state s;
      prob.logits.(prob.id) <- score;
      (output, s, params_dist)
    in

    (* Execute all the particles *)
    let values =
      Array.mapi (fun i -> particle_step { id = i; logits = state.scores })
        state.particles
    in
    let logits = state.scores in
    let results_dist = support ~values ~logits in

    (* Resample *)
    let particles =
      Array.init nb_particles (fun _ ->
          let s = alloc () in
          let _, new_s, new_phi = Distribution.draw results_dist in
          copy new_s s;
          (* Add guide params phi in the state *)
          s.params <- Some new_phi;
          s)
    in
    state.particles <- particles;
    Array.fill state.scores 0 nb_particles 0.;
    Array.iteri (fun i _ -> state.scores.(i) <- 0.) state.scores;

    (* Extract results *)
    let outputs =
      support
        ~values:(Array.map (fun (o, _, _) -> o) values)
        ~logits
    in
    let mixture =
      Distribution.to_mixture (support
        ~values:(Array.map
                   (fun (_, _, p) ->
                      let d, _ = Distribution.split (guide_dist guide p) in
                      d)
                   values)
        ~logits)
    in
    Distribution.of_pair (mixture, outputs)
  in

(*  let infer_step state (guide, dist, data) =
    let values =
      Array.mapi
        (fun i (s, thetas) ->
          let clean_step step ?(state_opt = Some s) work_state random_params =
            (match state_opt with
            | Some s -> copy s work_state (* start execution from state_opt *)
            | None -> () (* imperative execution, side-effect on work_state *));
            state.scores.(i) <- 0.;
            let prob = { id = i; logits = state.scores } in
            let _, output = step work_state (prob, random_params, data) in
            (output, state.scores.(i))
          in
          let thetas =
            match thetas with
            | Some thetas -> thetas
            | None ->
                reinforce
                  (Array.make (guide_size guide) 0.)
                  (Distribution.logpdf dist) guide eta batch iter
          in
          let prior = guide_dist guide thetas in
          let work_state = alloc () in
          let thetas =
            reinforce thetas
              (fun random_params ->
                let _, score =
                  clean_step
                    (fun s (prob, random_params, data) ->
                      observe (prob, prior, random_params);
                      step s (prob, random_params, data))
                    work_state random_params
                in
                score)
              guide eta batch iter
          in
          let theta = Distribution.draw prior in
          let output, _ = clean_step step ~state_opt:None s theta in
          (output, (s, thetas)))
        state.particles
    in
    let dists =
      Array.map (fun (_, (_, thetas)) -> guide_dist guide thetas) values
    in
    let mixture = Distribution.mixture ~dists ~logits:state.scores in
    let outputs, dist =
      Distribution.split (Distribution.support ~values ~logits:state.scores)
    in
    let particles =
      Array.init nb_particles (fun _ ->
          let s = alloc () in
          let new_s, new_thetas = Distribution.draw dist in
          copy new_s s;
          (s, Some new_thetas))
    in
    state.particles <- particles;
    Array.iteri (fun i _ -> state.scores.(i) <- 0.) state.scores;
    (mixture, outputs)
  in*)
  let infer_copy _ _ = assert false in
  Cnode
    {
      alloc = infer_alloc;
      reset = infer_reset;
      step = infer_step;
      copy = infer_copy;
    }
