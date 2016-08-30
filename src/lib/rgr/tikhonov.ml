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

open SolvedLPViaSvd
open Lacaml_util
open Lacaml.D

type opt =
  { tik_matrix : float array array
  ; l2_regularizer : [`S of float | `From of float array] option
  }

let opt ?(tik_matrix = [|[||]|]) ?l2_regularizer () =
  { tik_matrix ; l2_regularizer }

let default = opt ()

module E = Eval_multivariate
include E

let gtr_to_lambda fit_model l2_regularizer =
  let g slp = Vec.ssqr slp.looe in
  to_lambda fit_model g l2_regularizer

(* TODO: This method can be optimized if we use a different decomposition. *)
let gtk_solve_lp pred resp tik = function
  | None ->
      let covm = gemm ~transa:`T pred pred in
      Mat.axpy covm tik;
      getri tik;      (* take inverse with LU decomp, tik is overwritten. *)
      let coef = gemv (gemm tik ~transb:`T pred) resp in
      let resi = Vec.sub resp (gemv pred coef) in
      let looe = full_looe covm pred resi in
      { coef ; vaco = `Cov covm ; resi ; looe}
  | Some l2_regularizer ->
      let covm = gemm ~transa:`T pred pred in
      let eval l =
        let copy = lacpy covm in
        Mat.axpy ~alpha:l tik copy;
        getri copy;   (* take inverse with LU decomp, tik is overwritten. *)
        let coef = gemv (gemm copy ~transb:`T pred) resp in
        let resi = Vec.sub resp (gemv pred coef) in
        let looe = full_looe covm pred resi in
        { coef ; vaco = `Cov covm ; resi ; looe}
      in
      let lambda, slp = gtr_to_lambda eval l2_regularizer in
      let _ = P.printf "chose gtr lambda of %0.4f\n" lambda in
      slp

let regress ?(opt=default) pred ~resp =
  let pred = Mat.of_array pred in
  let resp = Vec.of_array resp in
  let lambda = opt.l2_regularizer in
  let tik =
    match opt.tik_matrix with
    | [|[||]|] -> Mat.make0 (Mat.dim1 pred) (Mat.dim2 pred)
    | tm       -> Mat.of_array tm
  in
  let num_obs  = Mat.dim1 pred in  (* rows *)
  let num_pred = Mat.dim2 pred in  (* cols *)
  let across_pred_col f = Array.init num_pred (fun i -> f (Mat.col pred (i + 1))) in
  let num_obs_float = float num_obs in
  let m_resp        = col_mean num_obs_float resp in
  let sum_sq_dm c m = Vec.ssqr (Vec.add_const (-.m) c) in
  let s_yy          = sum_sq_dm resp m_resp in
  let m_pred        = across_pred_col (col_mean num_obs_float) in
  (* since num_pred includes a value for the constant coefficient, no -1 is needed. *)
  let deg_of_freedom  = float (num_obs - num_pred) in
  let solved_lp       = gtk_solve_lp pred resp tik lambda in
  let sum_residuals   = dot solved_lp.resi solved_lp.resi in
  let inferred_var    = sum_residuals /. deg_of_freedom in
  let aic =
    let n = float num_obs in
    let k = float num_pred in
    2.0 *. k +. (log (sum_residuals /. n)) +. (n +. k) /. (n -. k -. 2.0)
  in
  { padded = false
  ; m_pred
  ; m_resp
  ; deg_of_freedom
  ; sum_residuals
  ; inferred_var
  ; s_yy
  ; solved_lp
  ; aic = aic
  }

let confidence_interval t ~alpha input =
  P.eprintf "Tikhonov confidence intervals are experimental, caution!\n";
  confidence_interval t ~alpha input

let prediction_interval t ~alpha input =
  P.eprintf "Tikhonov prediction intervals are experimental, caution!\n";
  prediction_interval t ~alpha input

let coefficient_tests ?null t =
  P.eprintf "Tikhonov coefficient tests are experimental, caution!\n";
  coefficient_tests ?null t

let f_statistic t =
  P.eprintf "Tikhonov f statistic are experimental, caution!\n";
  f_statistic t


