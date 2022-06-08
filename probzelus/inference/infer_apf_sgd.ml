include Infer_apf

type apf_params = {
  apf_particles : int;
  apf_iter : int;
  apf_eta : float;
  apf_batch : int;
}

module Sgd(P : sig val iter : int val eta : float val batch : int end) :
  REINFORCE =
struct
  type 'a guide = 'a Distribution.constraints
  type 'a t = float array

  let to_guide = guide

  let to_distribution = guide_dist

  let rec gradient_desc eta iter thetas f =
    if iter = 0 then thetas
    else
      let grad_step =
        Array.init P.batch (fun _ -> f thetas ())
        |> Owl.Mat.of_arrays |> Owl.Mat.sum ~axis:0
        |> fun g -> Owl.Mat.(g *$ (eta /. float P.batch))
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
      gradient_desc eta P.iter thetas
        (fun thetas () ->
          let dist = to_distribution q thetas in
          let vs = Distribution.draw dist in
          let q_thetas_vs = Distribution.score (dist, vs) in
          let d_q_thetas_vs = guide_logpdf q thetas vs in
          let logscore = logscore vs in
          Array.mapi
            (fun i _ -> d_q_thetas_vs.(i) *. (q_thetas_vs -. logscore)) thetas)
    with _ -> reinforce (eta /. 2.) q thetas logscore

  let reinforce q = reinforce P.eta q

  let init guide prior =
    reinforce guide (Array.make (guide_size guide) 0.)
      (fun v -> Distribution.score (prior, v))

  let reinforce q thetas dist logscore =
    reinforce q thetas (fun v -> logscore v +. Distribution.score (dist, v))
end

let infer { apf_particles; apf_iter; apf_eta; apf_batch } =
  let module P =
    struct
      let iter = apf_iter
      let eta = apf_eta
      let batch = apf_batch
    end
  in
  let module R = Sgd(P) in
  let module I = Make(R) in
  I.infer apf_particles
