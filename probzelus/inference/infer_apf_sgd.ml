include Infer_apf

type apf_params = {
  apf_particles : int;
  apf_iter : int;
  apf_eta : float;
  apf_batch : int;
  apf_is_particles : int;
}

module Sgd(P : sig val params : apf_params end) : REINFORCE = struct
  type 'a guide = 'a Distribution.constraints
  type 'a t = float array

  let to_guide = guide

  let to_distribution = guide_dist

  let rec gradient_desc eta iter thetas f =
    if iter = 0 then thetas
    else
      let grad_step =
        Array.init P.params.apf_batch (fun _ -> f thetas ())
        |> Owl.Mat.of_arrays |> Owl.Mat.sum ~axis:0
        |> fun g -> Owl.Mat.(g *$ (eta /. float P.params.apf_batch))
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
      gradient_desc eta (iter - 1) thetas f

  let rec reinforce eta q thetas logscore =
    try
      gradient_desc eta P.params.apf_iter thetas
        (fun thetas () ->
          let dist = to_distribution q thetas in
          let vs = Distribution.draw dist in
          let q_thetas_vs = Distribution.score (dist, vs) in
          let d_q_thetas_vs = guide_logpdf q thetas vs in
          let logscore = logscore vs in
          Array.mapi
            (fun i _ -> d_q_thetas_vs.(i) *. (q_thetas_vs -. logscore)) thetas)
    with _ -> reinforce (eta /. 2.) q thetas logscore

  let reinforce q = reinforce P.params.apf_eta q

  let init guide prior =
    reinforce guide (Array.make (guide_size guide) 0.)
      (fun v -> Distribution.score (prior, v))
end

let infer params =
  let module P = struct let params = params end in
  let module R = Sgd(P) in
  let module I = Make(R) in
  I.infer params.apf_particles
