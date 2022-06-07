include Infer_apf

module Adagrad(P : sig val params : apf_params end) : REINFORCE = struct
  type 'a guide = 'a Distribution.constraints
  type 'a t = float array

  let to_guide = guide

  let to_distribution = guide_dist

  let rec adagrad params thetas f grads =
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
      adagrad { params with apf_iter = params.apf_iter - 1 } thetas f grads

  let reinforce q thetas logscore =
    adagrad P.params thetas
      (fun thetas () ->
        let dist = to_distribution q thetas in
        let vs = Distribution.draw dist in
        let q_thetas_vs = Distribution.score (dist, vs) in
        let d_q_thetas_vs = guide_logpdf q thetas vs in
        let logscore = logscore vs in
        Array.mapi (fun i _ -> d_q_thetas_vs.(i) *. (q_thetas_vs -. logscore))
          thetas)
      (Owl.Mat.create 1 (Array.length thetas) 1e-8)

  let init guide prior =
    reinforce guide (Array.make (guide_size guide) 0.)
      (fun v -> Distribution.score (prior, v))
end

let infer params =
  let module P = struct let params = params end in
  let module R = Adagrad(P) in
  let module I = Make(R) in
  I.infer params.apf_particles
