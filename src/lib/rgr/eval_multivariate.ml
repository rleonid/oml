(*
   Copyright 2015,2016:
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

(* All the code of evaluating, not fitting a linear model goes here 

  Still a bit to do to figure out if all of these results are also
  true for Tikhonov. *)

open Util
open SolvedLPViaSvd
open Lacaml.D
module P = Printf
module D = Distributions
module Svd = Uncategorized.Svd

type glm =
  { padded            : bool
  ; m_pred            : float array
  ; m_resp            : float
  ; deg_of_freedom    : float
  ; sum_residuals     : float
  ; inferred_var      : float
  ; s_yy              : float
  ; solved_lp         : solved_lp
  (* ; d_w            : float
  Durbin Watson scores. [0..4] with a mean of 2.0 lower
  ( < 1) indicates positive correlation while
  higher (> 3) indicates negative correlation. *)
  ; aic               : float
  }

(* At some point I'd like to reavaluate the construction of the final
   Multivariate and Tikhonov modules and see if a Functor approach
   will work. But I had a bit of difficulty with unraveling the
   dependencies between the three types. I think this separation is fine
   from a development perspective. *)

type t = glm
type input = float array

let residuals glm = Vec.to_array glm.solved_lp.resi
let coefficients glm = Vec.to_array glm.solved_lp.coef

let describe glm =
  let coefs =
    coefficients glm
    |> Array.map (P.sprintf "%0.4f")
    |> Array.to_list
    |> String.concat "; "
  in
  if glm.padded then
    P.sprintf "%s^T * [|1;X|]" coefs
  else
    P.sprintf "%s^T * [|X|]" coefs

let eval glm vec =
  let dot = Array.fold2 (fun s x y -> s +. x *. y) 0.0 in
  let coe = coefficients glm in
  let n = Array.length coe in
  let vn = Array.length vec in
  if glm.padded then
    if vn + 1 <> n then
      invalid_arg ~m:"Regression" ~f:"eval"
        "Improper array length: %d exp %d - 1." vn n
    else
      let c = Array.sub coe 1 (n - 1) in
      coe.(0) +. dot c vec
  else if vn <> n then
    invalid_arg ~m:"Regression" ~f:"eval"
      "Improper array length: %d exp %d." vn n
  else
    dot coe vec

let confidence_interval, prediction_interval =
  let interval ~plus_one glm ~alpha x =
    let degrees_of_freedom = truncate glm.deg_of_freedom in
    let x_v =
      if glm.padded
      then Vec.of_list (1. :: (Array.to_list x))
      else Vec.of_array x
    in
    let p   =
      match glm.solved_lp.vaco with
      | `Cov c -> dot x_v (gemv c x_v)
      | `Svd s -> dot x_v (gemv (Svd.covariance_matrix_inv s) x_v)
    in
    let p' = if plus_one then 1. +. p else p in
    let sc = sqrt (glm.inferred_var  *. p') in
    let t  = D.student_quantile ~degrees_of_freedom (alpha /. 2.0) in
    let d  = sc *. (abs_float t) in
    let y  = eval glm x in
    (y -. d), (y +. d)
  in
  interval ~plus_one:false, interval ~plus_one:true

let residual_standard_error glm = sqrt glm.inferred_var

let coeff_of_determination glm = 1.0 -. glm.sum_residuals /. glm.s_yy

let coefficient_tests ?(null=0.0) glm =
  let coe = coefficients glm in
  let n = Array.length coe in
  let cov_dia =
    match glm.solved_lp.vaco with
    | `Cov c -> Mat.copy_diag c
    | `Svd s -> Mat.copy_diag (Svd.covariance_matrix_inv s)
  in
  let degrees_of_freedom = truncate glm.deg_of_freedom in
  Array.init n (fun i ->
    let error = sqrt (glm.inferred_var *. cov_dia.{i+1}) (*FORTRAN*) in
    let diff  = coe.(i) -. null in
    Hypothesis_test.(t_test Two_sided ~degrees_of_freedom ~diff ~error))

let f_statistic t =
  let n = Vec.dim t.solved_lp.coef in
  let p = if t.padded then float (n - 1) else float n in
  ((t.s_yy -. t.sum_residuals) /. p) /. (t.sum_residuals /. t.deg_of_freedom)

let aic t = t.aic

let press t = Vec.sqr_nrm2 (t.solved_lp.looe)
