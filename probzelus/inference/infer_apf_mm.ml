include Infer_apf

type apf_params = {
  apf_particles : int;
  apf_mm_particles : int;
}

module Moment_matching(P : sig val particles : int end) : REINFORCE = struct
  type 'a guide = 'a Distribution.constraints
  type 'a t = float array

  let to_guide = guide

  let to_distribution = guide_dist

  let rec moment_matching :
    type a. a guide -> a Distribution.t -> int -> float array -> unit =
    function
    | Dirac _ -> fun _ _ _ -> ()
    | Real ->
        fun d offset output ->
          let m, s = Distribution.stats_float d in
          output.(offset) <- m;
          output.(offset + 1) <- log s /. 2.
    | Interval _ -> assert false
    | Left_bounded _ -> assert false
    | Right_bounded _ -> assert false
    | Pair (g1, g2) ->
        fun d offset output ->
          let size_g1 = guide_size g1 in
          let d1, d2 = Distribution.split d in
          moment_matching g1 d1 offset output;
          moment_matching g2 d2 (offset + size_g1) output
    | List gs ->
        fun d offset output ->
          let _ =
            List.fold_left2
              (fun sizes g d ->
                 let size_g = guide_size g in
                 moment_matching g d sizes output;
                 sizes + size_g)
              offset gs (Distribution.split_list d)
          in
          ()
    | Array gs ->
        fun d offset output ->
          let _ =
            List.fold_left2
              (fun sizes g d ->
                 let size_g = guide_size g in
                 moment_matching g d sizes output;
                 sizes + size_g)
              offset (Array.to_list gs)
              (Array.to_list (Distribution.split_array d))
          in
          ()

  let moment_matching guide dist =
    let output = Array.make (guide_size guide) 0. in
    moment_matching guide dist 0 output;
    output

  let init guide prior = moment_matching guide prior

  let reinforce q thetas logscore =
    let dist = to_distribution q thetas in
    let values = Array.init P.particles (fun _ -> Distribution.draw dist) in
    let logits = Array.map logscore values in
    let _, dist = Normalize.normalize_nohist values logits in
    moment_matching q dist
end

let infer { apf_particles; apf_mm_particles } =
  let module P = struct let particles = apf_mm_particles end in
  let module R = Moment_matching(P) in
  let module I = Make(R) in
  I.infer apf_particles
