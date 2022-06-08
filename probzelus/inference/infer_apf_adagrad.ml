include Infer_apf

type apf_params = {
  apf_particles : int;
  apf_iter : int;
  apf_eta : float;
}

module Adagrad(P : sig val iter : int val eta : float end) : UPDATE = struct
  type 'a guide = 'a Distribution.constraints
  type 'a t = float array

  let to_guide = guide

  let to_distribution = guide_dist

  let rec adagrad iter thetas f grads =
    if iter = 0 then thetas
    else
      let grad =
        Array.init 1 (fun _ -> f thetas ())
        |> Owl.Mat.of_arrays |> Owl.Mat.sum ~axis:0
      in
      let grads = Owl.Mat.(grads + grad * grad) in
      let thetas =
        thetas
        |> (fun t -> Owl.Mat.of_array t 1 (Array.length t))
        |> Owl.Mat.(fun t -> t - grad / sqrt grads *$ P.eta)
        |> Owl.Mat.to_array
      in
      adagrad (iter - 1) thetas f grads

  let update q thetas prior logscore =
    adagrad P.iter thetas
      (fun thetas () ->
        let dist = to_distribution q thetas in
        let vs = Distribution.draw dist in
        let q_thetas_vs = Distribution.score (dist, vs) in
        let d_q_thetas_vs = guide_logpdf q thetas vs in
        let logscore = logscore vs +. Distribution.score (prior, vs) in
        Array.mapi (fun i _ -> d_q_thetas_vs.(i) *. (q_thetas_vs -. logscore))
          thetas)
      (Owl.Mat.create 1 (Array.length thetas) 1e-8)

  let init guide prior =
    update guide (Array.make (guide_size guide) 0.) prior (fun _ -> 0.)
end

let infer { apf_particles; apf_iter; apf_eta } =
  let module P = struct let iter = apf_iter let eta = apf_eta end in
  let module U = Adagrad(P) in
  let module I = Make(U) in
  I.infer apf_particles
