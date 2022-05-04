type _ guide =
  | Auto_unit : unit guide
  | Auto_unbounded : float guide
  | Auto_bounded : float * float -> float guide
  | Auto_left_bounded : float -> float guide
  | Auto_right_bounded : float -> float guide
  | Auto_pair : 'a guide * 'b guide -> ('a * 'b) guide
  | Auto_list : 'a guide list -> 'a list guide

let auto_unit = Auto_unit
let auto_unbounded = Auto_unbounded
let auto_bounded a b = Auto_bounded (a, b)
let auto_left_bounded a = Auto_left_bounded a
let auto_right_bounded b = Auto_right_bounded b
let auto_pair g1 g2 = Auto_pair (g1, g2)
let auto_list gs = Auto_list gs

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
  | Auto_unit -> 0
  | Auto_unbounded -> 2
  | Auto_bounded (_, _) -> guide_size Auto_unbounded
  | Auto_left_bounded _ -> guide_size Auto_unbounded
  | Auto_right_bounded _ -> guide_size Auto_unbounded
  | Auto_pair (g1, g2) -> guide_size g1 + guide_size g2
  | Auto_list gs -> List.fold_left (fun acc g -> acc + guide_size g) 0 gs

let transform d f f_prim f_inv =
  let sample _ = f (Distribution.draw d) in
  let logpdf y = let x = f_inv y in Distribution.score (d, x) -. log (f_prim x) in
  Distribution.sampler (sample, logpdf)

let rec guide_dist : type a. a guide -> float array -> a Distribution.t = function
  | Auto_unit -> fun _ -> Distribution.dirac ()
  | Auto_unbounded ->
      fun thetas ->
        Distribution.normal (thetas.(0), exp thetas.(1))
  | Auto_bounded (a, b) ->
      fun thetas ->
        transform
          (guide_dist Auto_unbounded thetas)
          (fun x -> a +. ((b -. a) /. (1. +. exp (-.x))))
          (fun x ->
            let exp_m_x = exp (-.x) in
            let one_plus_exp_m_x = 1. +. exp_m_x in
            (b -. a) *. exp_m_x /. (one_plus_exp_m_x *. one_plus_exp_m_x))
          (fun y -> -.log (((b -. a) /. (y -. a)) -. 1.))
  | Auto_left_bounded a ->
      fun thetas ->
        transform
          (guide_dist Auto_unbounded thetas)
          (fun x -> a +. exp x)
          (fun x -> exp x)
          (fun y -> log (y -. a))
  | Auto_right_bounded b ->
      fun thetas ->
        transform
          (guide_dist Auto_unbounded thetas)
          (fun x -> b -. exp (-.x))
          (fun x -> exp (-.x))
          (fun y -> -.log (b -. y))
  | Auto_pair (g1, g2) ->
      fun thetas ->
        let size_g1 = guide_size g1 in
        let size_g2 = guide_size g2 in
        let d1 = guide_dist g1 (Array.sub thetas 0 size_g1) in
        let d2 = guide_dist g2 (Array.sub thetas size_g1 size_g2) in
        Distribution.of_pair (d1, d2)
  | Auto_list gs ->
      fun thetas ->
        let _, ds =
          List.fold_left_map
            (fun acc g ->
              let size_g = guide_size g in
              (acc + size_g, guide_dist g (Array.sub thetas acc size_g)))
            0 gs
        in
        Distribution.of_list ds

let rec guide_logpdf : type a. a guide -> float array -> a -> float array =
  function
  | Auto_unit -> fun _ _ -> [||]
  | Auto_unbounded ->
      fun thetas v ->
        let v_minus_mu = v -. thetas.(0) in
        let sigma2 = exp (2. *. thetas.(1)) in
        [| v_minus_mu /. sigma2; (v_minus_mu *. v_minus_mu /. sigma2) -. 1. |]
  | Auto_bounded (a, b) ->
      fun thetas v ->
        guide_logpdf Auto_unbounded thetas
          (-.log (((b -. a) /. (v -. a)) -. 1.))
  | Auto_left_bounded a ->
      fun thetas v -> guide_logpdf Auto_unbounded thetas (log (v -. a))
  | Auto_right_bounded b ->
      fun thetas v -> guide_logpdf Auto_unbounded thetas (-.log (b -. v))
  | Auto_pair (g1, g2) ->
      fun thetas (v1, v2) ->
        let size_g1 = guide_size g1 in
        let size_g2 = guide_size g2 in
        Array.append
          (guide_logpdf g1 (Array.sub thetas 0 size_g1) v1)
          (guide_logpdf g2 (Array.sub thetas size_g1 size_g2) v2)
  | Auto_list gs ->
      fun thetas vs ->
        snd
          (List.fold_left2
             (fun (sizes, sum) g v ->
               let size_g = guide_size g in
               ( sizes + size_g,
                 Array.append sum
                   (guide_logpdf g (Array.sub thetas sizes size_g) v) ))
             (0, [||]) gs vs)

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

let infer_apf params (Cnode { alloc; reset; step; copy }) =
  let nb_particles = params.apf_particles in
  let eta = params.apf_eta in
  let batch = params.apf_batch in
  let iter = params.apf_iter in

  let infer_alloc () =
    {
      particles = Array.init nb_particles (fun _ -> (alloc (), None));
      scores = Array.make nb_particles 0.;
    }
  in
  let infer_reset state =
    Array.iteri
      (fun i (s, _) ->
        reset s;
        state.particles.(i) <- (s, None))
      state.particles;
    Array.iteri (fun i _ -> state.scores.(i) <- 0.) state.scores
  in

  let infer_step state (guide, params_prior, data) =
    let particle_step idx (s, phi) =
      (* 0. Get guide parameter from state *)
      let phi =
        match phi with
        | Some phi -> phi
        | None ->
            reinforce
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
        state.scores.(idx) <- 0.;
        let prob = { id = idx; logits = state.scores } in
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
        let _, output = step work_state (prob, (theta, data)) in
        (output, work_state, state.scores.(idx))
      in

      (* 2. Sample the next value, state, and score from the model *)
      let output, work_state, score = model_step None in

      (* 4. Reinforce params_dist using the model as a function of params *)
      let params_dist =
        reinforce phi
          (fun params ->
            let _, _, score = model_step (Some params) in
            score)
          guide eta batch iter
      in

      (* 5. Restore the state *)
      copy work_state s;
      state.scores.(idx) <- score;
      (output, s, params_dist)
    in

    (* Execute all the particles *)
    let values = Array.mapi particle_step state.particles in
    let logits = state.scores in
    let results_dist = support ~values ~logits in

    (* Resample *)
    let particles =
      Array.init nb_particles (fun _ ->
          let s = alloc () in
          let _, new_s, new_phi = Distribution.draw results_dist in
          copy new_s s;
          (* Add guide params phi in the state *)
          (s, Some new_phi))
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
        ~values:(Array.map (fun (_, _, p) -> guide_dist guide p) values)
        ~logits)
    in
    (mixture, outputs)
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
