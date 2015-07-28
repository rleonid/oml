
open Util
open Descriptive
open Functions

let prediction_interval_sub k std mean alpha =
  let t_v = Distributions.student_quantile (k - 1) (alpha /. 2.) in
  let fk  = float k in
  let d   = Float.(t_v * std * sqrt (1. + (1. / fk))) in
  (mean -. d, mean +. d)

let prediction_interval dist_stat =
  prediction_interval_sub dist_stat.size dist_stat.std dist_stat.mean

type test =
  { standard_error      : float
  ; degrees_of_freedom  : float   (* can be non-integer due to corrections. *)
  ; stat                : float
  ; prob_by_chance      : float
  }

let test_to_string t =
  Printf.sprintf "standard error: %.3f, degrees of freedom: %.3f, \
                  stat: %.3f, probability observation by chance: %.3f"
        t.standard_error t.degrees_of_freedom t.stat t.prob_by_chance

let chi observed expected =
  let dgf = Array.length expected - 1 in
  let stat =
    Array.map2 (fun o e -> (o -. e) *. (o -. e) /. e) observed expected
    |> Array.sumf
  in
  let degrees_of_freedom = float dgf in
  { degrees_of_freedom  ; stat
  ; prob_by_chance      = chi_square_greater dgf stat
  ; standard_error      = sqrt (2.0 *. degrees_of_freedom)
  }

type null_hypothesis =
  | TwoTail
  | OneTail

let simple_t_test hypothesis degrees_of_freedom standard_error stat_diff =
  let stat = stat_diff /. standard_error in
  let prob_by_chance =
    let p = 1.0 -. (student_t_less degrees_of_freedom
      (abs_float stat))
    in
    match hypothesis with
    | TwoTail -> p
    | OneTail -> p /. 2.0
  in
  { standard_error ; stat ; prob_by_chance
  ; degrees_of_freedom = float_of_int degrees_of_freedom;
  }

(* I am going to ignore the tests where you 'know' the population variance. *)

let mean_t_test population_mean hypothesis arr =
  let m = mean arr in
  let std = sqrt (unbiased_var arr) in
  let n = float (Array.length arr) in
  let se = std /. (sqrt n) in
  let stat = (m -. population_mean) /. se in
  let deg_free = n -. 1.0 in
  let prob_by_chance =
    match hypothesis with
    | TwoTail -> 1.0 -. (student_t_less (truncate deg_free) (abs_float stat))
    | OneTail -> (1.0 -. (student_t_less (truncate deg_free) (abs_float stat))) /. 2.0
  in
  { standard_error = se
  ; degrees_of_freedom = deg_free
  ; stat
  ; prob_by_chance
  }

let equal_means_same_variance_test hypothesis arr1 arr2 =
  let m1 = mean arr1 in
  let m2 = mean arr2 in
  let n1 = float (Array.length arr1) in
  let n2 = float (Array.length arr2) in
  let deg_free = n1 +. n2 -. 2.0 in
  (* standard error. *)
  let se =
    let mo1 = moment 2 arr1 in
    let mo2 = moment 2 arr2 in
    let num = Float.((n1 - 1.0) * mo1 + (n2 - 1.0) * mo2) in
    let den = Float.(deg_free * ((1.0 / n1) + (1.0 / n2))) in
    sqrt (num /. den)
  in
  let stat = (m1 -. m2) /. se in
  let prob_by_chance =
    match hypothesis with
    | TwoTail -> 1.0 -. (student_t_less (truncate deg_free) (abs_float stat));
    | OneTail -> (1.0 -. (student_t_less (truncate deg_free) (abs_float stat)) /. 2.0);
  in
  { standard_error = se
  ; degrees_of_freedom = deg_free
  ; stat
  ; prob_by_chance
  }

let unequal_variance_test hypothesis arr1 arr2 =
  let m1 = mean arr1 in
  let m2 = mean arr2 in
  let n1 = float (Array.length arr1) in
  let n2 = float (Array.length arr2) in
  let vn1 = (var arr1) /. n1 in
  let vn2 = (var arr2) /. n2 in
  (* standard error, kind of ? *)
  let se = sqrt (vn1 +. vn2) in
  let stat = (m1 -. m2) /. se in
  let deg_free = ((vn1 +. vn2) ** 2.0) /.
    ((vn1 ** 2.0) /. (n1 -. 1.0) +. (vn2 ** 2.0) /. (n2 -. 1.0))
  in
  let se =
    let mo1 = moment 2 arr1 in
    let mo2 = moment 2 arr2 in
    let num = Float.((n1 - 1.0) * mo1 + (n2 - 1.0) * mo2) in
    let den = Float.(deg_free * ((1.0 / n1) + (1.0 / n2))) in
    sqrt (num /. den)
  in
  (* this distribution according to Numerical recipes is approximately Student. *)
  let prob_by_chance =
    match hypothesis with
    | TwoTail -> 1.0 -. (student_t_less (truncate deg_free) (abs_float stat));
    | OneTail -> (1.0 -. (student_t_less (truncate deg_free) (abs_float stat)) /. 2.0);
  in
  { standard_error = se
  ; degrees_of_freedom = deg_free
  ; stat
  ; prob_by_chance = prob_by_chance
  }

let different_variances_test arr1 arr2 =
  let v1 = var arr1 in
  let v2 = var arr2 in
  let f, dgf1, dgf2 =
    if v1 > v2 then
      v1 /. v2, float (Array.length arr1), float (Array.length arr2)
    else
      v2 /. v1, float (Array.length arr2), float (Array.length arr1)
  in
  let p = 2.0 *. f_less dgf1 dgf2 f in
  let p = if p > 1.0 then 2.0 -. p else p in
  { standard_error = 0.0
  ; degrees_of_freedom = dgf1 +. dgf2
  ; stat = f
  ; prob_by_chance = p
  }

let correlation_test arr1 arr2 =
  let r = correlation arr1 arr2 in
  (* test of correlation assumes that the sizes are the same. *)
  let deg_free = float (Array.length arr1 - 2) in
  let stat = r *. sqrt ( deg_free /. (1.0 -. r *. r)) in
  { standard_error = 0.0
  ; degrees_of_freedom = deg_free
  ; stat
  ; prob_by_chance = 1.0 -. (student_t_less (truncate deg_free) (abs_float stat));
  }
