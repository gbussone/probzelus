include Infer_apf

type apf_params = {
  apf_particles : int;
  apf_is_particles : int;
}

module Importance_sampling(P : sig val particles : int end) : UPDATE = struct
  type 'a guide = 'a array
  type 'a t = float array

  let to_guide prior =
    Array.init P.particles (fun _ -> Distribution.draw prior)

  let to_distribution values logits =
    let _, dist = Normalize.normalize_nohist values logits in
    dist

  let init _ _ = Array.make P.particles 0.

  let update values logits _ logscore =
    Array.map2 (fun v s -> s +. logscore v) values logits
end

let infer { apf_particles; apf_is_particles } =
  let module P = struct let particles = apf_is_particles end in
  let module U = Importance_sampling(P) in
  let module I = Make(U) in
  I.infer apf_particles
