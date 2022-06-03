include Infer_apf

module Importance_sampling : REINFORCE = struct
  type 'a guide = unit
  type 'a t = 'a array * float array

  let to_guide _ = ()

  let to_distribution () (values, logits) =
    let _, dist = Normalize.normalize_nohist values logits in
    dist

  let init params () prior =
    let values =
      Array.init params.apf_is_particles (fun _ -> Distribution.draw prior)
    in
    let logits = Array.make params.apf_particles 0. in
    values, logits

  let reinforce _params () (values, logits) logscore =
    let logits = Array.map2 (fun v s -> s +. logscore v) values logits in
    values, logits
end

module Infer = Make(Importance_sampling)
include Infer
