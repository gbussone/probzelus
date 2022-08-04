include Infer_fm

type fm_params = {
  fm_particles : int;
  fm_iter : int;
  fm_eta : float;
  fm_batch : int;
}

module Sgd(P : sig val iter : int val eta : float val batch : int end) :
  UPDATE =
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

  let rec update eta q thetas prior logscore =
    try
      gradient_desc eta P.iter thetas
        (fun thetas () ->
          let dist = to_distribution q thetas in
          let vs = Distribution.draw dist in
          let q_thetas_vs = Distribution.score (dist, vs) in
          let d_q_thetas_vs = guide_logpdf q thetas vs in
          let logscore = logscore vs +. Distribution.score (prior, vs) in
          Array.mapi
            (fun i _ -> d_q_thetas_vs.(i) *. (q_thetas_vs -. logscore)) thetas)
    with _ -> update (eta /. 2.) q thetas prior logscore

  let update q = update P.eta q

  let init guide prior =
    update guide (Array.make (guide_size guide) 0.) prior (fun _ -> 0.)
end

let infer { fm_particles; fm_iter; fm_eta; fm_batch } =
  let module P =
    struct
      let iter = fm_iter
      let eta = fm_eta
      let batch = fm_batch
    end
  in
  let module U = Sgd(P) in
  let module I = Make(U) in
  I.infer fm_particles
