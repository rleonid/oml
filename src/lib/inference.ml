
open Util
open Descriptive
open Distributions

let prediction_interval_sub k std mean alpha =
  let t_v = student_quantile ~k:(k - 1) (1.0 -. alpha /. 2.) in
  let fk  = float k in
  let d   = Float.(t_v * std * sqrt (1. + (1. / fk))) in
  (mean -. d, mean +. d)

let prediction_interval dist_stat =
  prediction_interval_sub dist_stat.size dist_stat.std dist_stat.mean

type test =
  { degrees_of_freedom  : float   (* can be non-integer due to corrections. *)
  ; statistic           : float
  ; standard_error      : float
  ; prob_by_chance      : float
  }

let test_to_string t =
  Printf.sprintf "degrees of freedom: %.3f\
                 , statistic: %.3f\
                 , standard error: %.3f\
                 , probability of chance observation: %.3f"
        t.degrees_of_freedom
        t.statistic
        t.standard_error
        t.prob_by_chance

(* TODO: Refactor the Chi logic to use a cdf *)
let chi observed expected =
  let dgf = Array.length expected - 1 in
  let statistic =
    Array.map2 (fun o e -> (o -. e) *. (o -. e) /. e) observed expected
    |> Array.sumf
  in
  let degrees_of_freedom = float dgf in
  { degrees_of_freedom
  ; statistic
  ; standard_error      = sqrt (2.0 *. degrees_of_freedom)
  ; prob_by_chance      = Functions.chi_square_greater dgf statistic
  }

(* TODO: Add Tests where the population variance is known, ie Z-tests.*)

type null_hypothesis =
  | TwoSided
  | OneSided

let t_test hypothesis degrees_of_freedom ~diff ~error =
  let statistic = diff /. error in
  let prob_by_chance =
    let upper_ct = student_cdf ~k:degrees_of_freedom (abs_float statistic) in
    let prob_upper_tail = 1.0 -. upper_ct in
    match hypothesis with
    | TwoSided -> prob_upper_tail *. 2.0
    | OneSided -> prob_upper_tail
  in
  { degrees_of_freedom = float_of_int degrees_of_freedom
  ; statistic
  ; standard_error = error
  ; prob_by_chance
  }

let mean_t_test population_mean hypothesis arr =
  let m = mean arr in
  let d = Array.length arr in
  let nf = float d in
  let sd = sqrt (unbiased_var arr) in
  let error = sd /. (sqrt nf) in
  let diff  = m -. population_mean in
  t_test hypothesis (d - 1) ~diff ~error

let means_same_variance_test hypothesis arr1 arr2 =
  let n1 = Array.length arr1 in
  let n2 = Array.length arr2 in
  let error =   (* standard error. *)
    let nf1 = float n1 in
    let nf2 = float n2 in
    let df1 = nf1 -. 1. in
    let df2 = nf2 -. 1. in
    let v1 = unbiased_var arr1 in
    let v2 = unbiased_var arr2 in
    let vp = Float.((v1 * df1 + v2 * df2) / (df1 + df2)) in
    let f  = Float.( 1. / nf1 + 1. / nf2) in
    sqrt (vp *. f)
  in
  let diff =
    let m1 = mean arr1 in
    let m2 = mean arr2 in
    m1 -. m2
  in
  let df = n1 + n2 - 2 in
  t_test hypothesis df ~diff ~error

(* TODO: probably easier to have one function with parameter for both? *)

let means_different_variance_test hypothesis arr1 arr2 =
  let n1 = Array.length arr1 in
  let n2 = Array.length arr2 in
  let nf1 = float n1 in
  let nf2 = float n2 in
  let w1 = unbiased_var arr1 /. nf1 in
  let w2 = unbiased_var arr2 /. nf2 in
  let vw = w1 +. w2 in
  let df1 = nf1 -. 1. in
  let df2 = nf2 -. 1. in
  let dg = Float.((vw * vw) / (w1 * w1 / df1 + w2 * w2 / df2)) in
  let error = sqrt vw in
  let diff =
    let m1 = mean arr1 in
    let m2 = mean arr2 in
    m1 -. m2
  in
  (* Can't find a reference for why you round down the dg, but probably better
    than rounding up! *)
  t_test hypothesis (truncate dg) ~diff ~error

let different_variances_test arr1 arr2 =
  let v1 = var arr1 in
  let v2 = var arr2 in
  let f, dgf1, dgf2 =
    if v1 > v2 then
      v1 /. v2, float (Array.length arr1), float (Array.length arr2)
    else
      v2 /. v1, float (Array.length arr2), float (Array.length arr1)
  in
  let p = 2.0 *. Functions.f_less dgf1 dgf2 f in
  let p = if p > 1.0 then 2.0 -. p else p in
  { standard_error = 0.0
  ; degrees_of_freedom = dgf1 +. dgf2
  ; statistic = f
  ; prob_by_chance = p
  }

let correlation_test arr1 arr2 =
  let r = correlation arr1 arr2 in
  (* test of correlation assumes that the sizes are the same. *)
  let deg_free = float (Array.length arr1 - 2) in
  let statistic = r *. sqrt ( deg_free /. (1.0 -. r *. r)) in
  { standard_error = 0.0
  ; degrees_of_freedom = deg_free
  ; statistic
  ; prob_by_chance = 1.0 -. (student_quantile ~k:(truncate deg_free) (abs_float statistic));
  }
