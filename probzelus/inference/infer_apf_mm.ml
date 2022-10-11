include Infer_apf

type apf_params = {
  apf_particles : int;
  apf_mm_particles : int;
}

module Moment_matching(P : sig val particles : int end) : UPDATE = struct
  type 'a guide = 'a Distribution.constraints
  type 'a t = float array

  let to_guide = guide

  let rec to_distribution :
    type a. a guide -> float array -> int -> a Distribution.t =
    function
    | Dirac x -> fun _ _ -> Distribution.dirac x
    | Bool -> fun thetas offset -> Distribution.bernoulli thetas.(offset)
    | Real ->
        fun thetas offset ->
          if thetas.(offset + 1) < 1e-10 then
            Distribution.dirac thetas.(offset)
          else
            Distribution.gaussian (thetas.(offset), thetas.(offset + 1))
    | Interval (a, b) ->
        fun thetas offset ->
          Distribution.add
            (Distribution.dirac a,
             Distribution.mult
               (Distribution.dirac (b -. a),
                if thetas.(offset) = 0. then
                  Distribution.dirac thetas.(offset + 1)
                else
                  Distribution.beta (thetas.(offset), thetas.(offset + 1))))
    | Left_bounded a ->
        fun thetas offset ->
          Distribution.add
            (Distribution.dirac a,
             Distribution.lognormal (thetas.(offset), thetas.(offset + 1)))
    | Right_bounded b ->
        fun thetas offset ->
          Distribution.add
            (Distribution.dirac b,
             Distribution.mult
               (Distribution.dirac (-.1.),
                Distribution.lognormal (thetas.(offset), thetas.(offset + 1))))
    | Pair (g1, g2) ->
        fun thetas offset ->
          let size_g1 = guide_size g1 in
          let d1 = to_distribution g1 thetas offset in
          let d2 = to_distribution g2 thetas (offset + size_g1) in
          Distribution.of_pair (d1, d2)
    | List gs ->
        fun thetas offset ->
          let _, ds =
            List.fold_left_map
              (fun acc g ->
                let size_g = guide_size g in
                (acc + size_g, to_distribution g thetas acc))
              offset gs
          in
          Distribution.of_list ds
    | Array gs ->
        fun thetas offset ->
          let _, ds =
            List.fold_left_map
              (fun acc g ->
                let size_g = guide_size g in
                (acc + size_g, to_distribution g thetas acc))
              offset (Array.to_list gs)
          in
          Distribution.of_array (Array.of_list ds)
    | Mv_gaussian n ->
        fun theta offset ->
          let mu = Owl.Mat.init n 1 (fun i -> theta.(offset + i)) in
          let sigma =
            Owl.Mat.init_2d n n (fun i j -> theta.(offset + n + i * n + j))
          in
          Distribution.mv_gaussian (mu, sigma)

  let to_distribution guide thetas = to_distribution guide thetas 0

  let stats_float d =
    let m, v = Distribution.stats_float d in
    m, v *. float_of_int P.particles /. float_of_int (P.particles - 1)

  let rec moment_matching :
    type a. a guide -> a Distribution.t -> int -> float array -> unit =
    function
    | Dirac _ -> fun _ _ _ -> ()
    | Bool ->
        fun d offset output ->
          let m = Distribution.mean_bool d in
          output.(offset) <- max 0. (min 1. m)
    | Real ->
        fun d offset output ->
          let m, v = stats_float d in
          output.(offset) <- m;
          output.(offset + 1) <- v
    | Interval (a, b) ->
        fun d offset output ->
          let m, v = stats_float d in
          let b_minus_a = b -. a in
          let m = (m -. a) /. b_minus_a in
          let v = v /. (b_minus_a *. b_minus_a) in
          if v < 1e-10 then begin
            (* when variance is too low, treat it as a Dirac *)
            (* by convention, alpha is set to 0, beta is set to the mean *)
            output.(offset) <- 0.;
            output.(offset + 1) <- m
          end else
            let scale = m *. (1. -. m) /. v -. 1. in
            output.(offset) <- m *. scale;
            output.(offset + 1) <- (1. -. m) *. scale
    | Left_bounded a ->
        fun d offset output ->
          let m, v = stats_float d in
          let m = m -. a in
          let sigma2 = log (1. +. v /. (m *. m)) in
          output.(offset) <- log m -. sigma2 /. 2.;
          output.(offset + 1) <- sqrt sigma2
    | Right_bounded b ->
        fun d offset output ->
          let m, v = stats_float d in
          let m = b -. m in
          let sigma2 = log (1. +. v /. (m *. m)) in
          output.(offset) <- log m -. sigma2 /. 2.;
          output.(offset + 1) <- sqrt sigma2
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
    | Mv_gaussian n ->
        fun d offset output ->
          match d with
          | Dist_mv_gaussian (mu, sigma, _) ->
              for i = 0 to n - 1 do
                output.(offset + i) <- Owl.Mat.get mu i 0
              done;
              for i = 0 to n - 1 do
                for j = 0 to n - 1 do
                  output.(offset + n + i * n + j) <- Owl.Mat.get sigma i j
                done
              done
          | _ -> match Distribution.to_dist_support d with
            | Dist_support l ->
                let means =
                  Array.init n
                    (fun i ->
                       List.fold_left
                         (fun sum (x, w) -> sum +. w *. Owl.Mat.get x i 0) 0. l
                       /. float_of_int n)
                in
                let covs =
                  Array.init n
                    (fun i ->
                       Array.init n
                         (fun j ->
                            List.fold_left
                              (fun sum (x, w) ->
                                 sum
                                 +. w *. Owl.Mat.get x i 0
                                    *. Owl.Mat.get x j 0)
                              0. l
                            /. float_of_int n
                            -. means.(i) *. means.(j)))
                in
                for i = 0 to n - 1 do
                  output.(offset + i) <- means.(i)
                done;
                for i = 0 to n - 1 do
                  for j = 0 to n - 1 do
                    output.(offset + n + i * n + j) <- covs.(i).(j)
                  done
                done
            | _ -> assert false

  let moment_matching guide dist =
    let output = Array.make (guide_size guide) 0. in
    moment_matching guide dist 0 output;
    output

  let init guide prior = moment_matching guide prior

  let update q _ dist logscore =
    let values = Array.init P.particles (fun _ -> Distribution.draw dist) in
    let logits = Array.map logscore values in
    let _, dist = Normalize.normalize_nohist values logits in
    moment_matching q dist
end

let infer { apf_particles; apf_mm_particles } =
  let module P = struct let particles = apf_mm_particles end in
  let module U = Moment_matching(P) in
  let module I = Make(U) in
  I.infer apf_particles apf_mm_particles
