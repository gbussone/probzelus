include Infer_fm

type fm_params = {
  fm_particles : int;
  fm_is_particles : int;
}

module Importance_sampling(P : sig val particles : int end) : UPDATE = struct
  type 'a guide = unit
  type 'a t = 'a array * float array

  let to_guide _ = ()

  let to_distribution () (values, logits) =
    let _, dist = Normalize.normalize_nohist values logits in
    dist

  let init () prior =
    let values = Array.init P.particles (fun _ -> Distribution.draw prior) in
    let logits = Array.make P.particles 0. in
    values, logits

  let update () (values, logits) _ logscore =
    let logits = Array.map2 (fun v s -> s +. logscore v) values logits in
    values, logits
end

let infer { fm_particles; fm_is_particles } =
  let module P = struct let particles = fm_is_particles end in
  let module U = Importance_sampling(P) in
  let module I = Make(U) in
  I.infer fm_particles
