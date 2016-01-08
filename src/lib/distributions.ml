(*
   Copyright 2015:
     Leonid Rozenberg <leonidr@gmail.com>
     Carmelo Piccione <carmelo.piccione@gmail.com>

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
  if alpha <= 0.0 then invalidArg "alpha" else
    if beta <= 0.0 then invalidArg "beta" else
      let alpha_minus_1 = alpha -. 1.0 in
      let beta_minus_1 = beta -. 1.0 in
      let z = F.ln_beta alpha beta in
      let zero_to_one_o = Open 0., Open 1. in
      fun x ->
        if not (within zero_to_one_o x) then neg_infinity else
          let logX = log x in
          let log1_minus_x = log1p (-.x) in
          alpha_minus_1 *. logX +. beta_minus_1 *. log1_minus_x -. z

let beta_pdf ~alpha ~beta =
  let ln_pdf = ln_beta_pdf ~alpha ~beta in
  fun x -> let value = ln_pdf x in
  if value = neg_infinity then 0.0 else exp value

let beta_cdf ~alpha ~beta =
  if alpha <= 0.0 then invalidArg "alpha" else
    if beta <= 0.0 then invalidArg "beta" else
      let reg = F.regularized_beta ~alpha ~beta in
      fun x -> if x <= 0.0 then 0.0 else if x >= 1.0 then 1.0 else reg x

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

let ln_dirichlet_pdf ~alphas =
  if alphas = [||] || Array.any (fun a -> a <= 0.0) alphas then
    invalidArg "alphas"
  else
    let alpha_m_one = Array.map (fun a -> a -. 1.) alphas in
    let norm = -1. *. F.ln_multivariate_beta alphas in
    let k = Array.length alphas in
    let check b s = if b then invalidArg "probabilities: %s" s else () in
    fun parr ->
      check (Array.length parr <> k) "different length from alphas";
      check (significantly_different_from (Array.sumf parr) 1.) "doesn't sum to 1";
      check (Array.any (fun p -> p < 0.0) parr) "probability less than zero";
      let s = ref norm in
      for i = 1 to k - 1 do
        s := !s +. log parr.(i) *. alpha_m_one.(i)
      done;
      !s

let dirichlet_pdf ~alphas =
  let ln_pdf = ln_dirichlet_pdf ~alphas in
  fun arr -> exp (ln_pdf arr)
