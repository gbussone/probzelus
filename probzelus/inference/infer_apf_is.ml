include Infer_apf

module Importance_sampling(P : sig val params : apf_params end) : REINFORCE = struct
  type 'a guide = unit
  type 'a t = 'a array * float array

  let to_guide _ = ()

  let to_distribution () (values, logits) =
    let _, dist = Normalize.normalize_nohist values logits in
    dist

  let init () prior =
    let values =
      Array.init P.params.apf_is_particles (fun _ -> Distribution.draw prior)
    in
    let logits = Array.make P.params.apf_particles 0. in
    values, logits

  let reinforce () (values, logits) logscore =
    let logits = Array.map2 (fun v s -> s +. logscore v) values logits in
    values, logits
end

let infer params =
  let module P = struct let params = params end in
  let module R = Importance_sampling(P) in
  let module I = Make(R) in
  I.infer params
