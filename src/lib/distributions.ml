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
module F = Functions

let normal_cdf ?(mean=0.0) ?(std=1.0) x =
  let z = ((x -. mean) /. std) in
  (1.0 +. F.erf (z /. sqrt 2.0)) /. 2.0

let normal_pdf ?(mean=0.0) ?(std=1.0) x =
  let z = ((x -. mean) /. std) in
  (exp ((-1.0 /. 2.0) *. (z ** 2.0))) /.  (std *. (sqrt (2.0 *. pi)))

let normal_quantile ?(mean=0.0) ?(std=1.0) p =
  if p < 0. || p > 1. then invalidArg "normal_quantile p %f" p else
    mean +. std *. F.normal_cdf_inv p

let poisson_cdf ~mean k =
  F.regularized_upper_gamma ~a:(floor (k +. 1.0)) mean

let ln_beta_pdf ~alpha ~beta =
  if alpha <= 0.0 then raise (Invalid_argument "alpha") else
    if beta <= 0.0 then raise (Invalid_argument "beta") else
      let alpha_minus_1 = alpha -. 1.0 in
      let beta_minus_1 = beta -. 1.0 in
      let z = F.ln_beta alpha beta in fun x ->
        if x < 0.0 || x > 1.0 then neg_infinity else
          if x = 0.0 then
            (if (alpha < 1.0) then raise (Invalid_argument "alpha") else neg_infinity)
            else if x = 1.0 then (if (beta < 1.0) then raise (Invalid_argument "beta")
          else neg_infinity) else
            let logX = log x in
            let log1_minus_x = log1p (-.x) in
            alpha_minus_1 *. logX +. beta_minus_1 *. log1_minus_x -. z

let beta_pdf ~alpha ~beta =
  let ln_pdf = ln_beta_pdf ~alpha ~beta in
  fun x -> let value = ln_pdf x in
  if value = neg_infinity then 0.0 else exp value

let beta_cdf ~alpha ~beta =
  if alpha <= 0.0 then raise (Invalid_argument "alpha") else
    if beta <= 0.0 then raise (Invalid_argument "beta") else
      let reg = F.regularized_beta ~alpha ~beta in
      let z = Functions.beta alpha beta in
      fun x -> if x <= 0.0 then 0.0 else if x >= 1.0 then 1.0 else
        (reg x) /. z

let chi_square_cdf ~k x =
  F.regularized_lower_gamma ~a:((float k) /. 2.0) (x /. 2.0)

(* According to Wikipedia, haven't research a more efficient algorithm.
   Implemented it here for completeness.
   TODO: Replace with OCephes versions? *)
let student_pdf ~degrees_of_freedom t =
  let v = float degrees_of_freedom in
  let e = -. (v+.1.)/.2. in
  Float.(((1. + (t * t / v)) **e) / ((sqrt v) * F.beta 0.5 (v / 2.)))

let student_cdf ~degrees_of_freedom t =
  let v = float degrees_of_freedom in
  let x = Float.(v / (t * t + v)) in
  if t > 0.0 then
    Float.(1.0 - 0.5 * (F.regularized_beta ~alpha:(v/2.) ~beta:0.5 x))
  else if t < 0.0 then
    Float.(0.5 * (F.regularized_beta ~alpha:(v/2.) ~beta:0.5 x))
  else  (* 0.0 *)
    0.5

let student_quantile ~degrees_of_freedom p =
  if p < 0. || p > 1. then invalidArg "student_quantile p %f" p else
    F.student_cdf_inv ~degrees_of_freedom p
