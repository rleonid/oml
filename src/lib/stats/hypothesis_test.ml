(*
   Copyright 2015:
     Leonid Rozenberg <leonidr@gmail.com>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

open Util
open Descriptive
open Distributions
module F = Oml_functions

let prediction_interval_sub k std mean alpha =
  let t_v = student_quantile ~degrees_of_freedom:(k - 1) (1.0 -. alpha /. 2.) in
  let fk  = float k in
  let d   = Float.(t_v * std * sqrt (1. + (1. / fk))) in
  (mean -. d, mean +. d)

let prediction_interval dist_stat =
  prediction_interval_sub dist_stat.size dist_stat.std dist_stat.mean

type t =
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
  ; prob_by_chance      = F.chi_square_greater dgf statistic
  }

(* TODO: Add Tests where the population variance is known, ie Z-tests.*)

type null_hypothesis =
  | Two_sided
  | One_sided

let t_test hypothesis ~degrees_of_freedom ~diff ~error =
  let statistic = diff /. error in
  let prob_by_chance =
    let upper_ct = student_cdf ~degrees_of_freedom (abs_float statistic) in
    let prob_upper_tail = 1.0 -. upper_ct in
    match hypothesis with
    | Two_sided -> prob_upper_tail *. 2.0
    | One_sided -> prob_upper_tail
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
  let sd = sd arr in
  let error = sd /. (sqrt nf) in
  let diff  = m -. population_mean in
  t_test hypothesis ~degrees_of_freedom:(d - 1) ~diff ~error

let means_same_variance_test hypothesis arr1 arr2 =
  let n1 = Array.length arr1 in
  let n2 = Array.length arr2 in
  let error =   (* standard error. *)
    let nf1 = float n1 in
    let nf2 = float n2 in
    let df1 = nf1 -. 1. in
    let df2 = nf2 -. 1. in
    let v1 = var arr1 in
    let v2 = var arr2 in
    let vp = Float.((v1 * df1 + v2 * df2) / (df1 + df2)) in
    let f  = Float.( 1. / nf1 + 1. / nf2) in
    sqrt (vp *. f)
  in
  let diff =
    let m1 = mean arr1 in
    let m2 = mean arr2 in
    m1 -. m2
  in
  let degrees_of_freedom = n1 + n2 - 2 in
  t_test hypothesis ~degrees_of_freedom ~diff ~error

(* TODO: probably easier to have one function with parameter for both? *)

let means_different_variance_test hypothesis arr1 arr2 =
  let n1 = Array.length arr1 in
  let n2 = Array.length arr2 in
  let nf1 = float n1 in
  let nf2 = float n2 in
  let w1 = var arr1 /. nf1 in
  let w2 = var arr2 /. nf2 in
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
  let degrees_of_freedom = truncate dg in
  t_test hypothesis ~degrees_of_freedom ~diff ~error

let variance_ratio_test arr1 arr2 =
  let v1 = var arr1 in
  let v2 = var arr2 in
  let dg1 = float (Array.length arr1 - 1) in
  let dg2 = float (Array.length arr2 - 1) in
  let f, d1, d2 =
    if v1 > v2 then
      v1 /. v2, dg1, dg2
    else
      v2 /. v1, dg2, dg1
  in
  let p = 2.0 *. F.f_less ~d1 ~d2 f in
  let p = if p > 1.0 then 2.0 -. p else p in
  { degrees_of_freedom = d1 +. d2
  ; statistic = f
  ; standard_error = nan
  ; prob_by_chance = p
  }
