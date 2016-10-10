
open Omlf_solvedLPViaSvd
open Omlf_lacaml_util
open Lacaml.D

module E = Omlf_eval_multivariate
include E

type opt =
  { add_constant_column : bool
  ; l2_regularizer : [`S of float | `From of float array] option
  }

let opt ?l2_regularizer ?(add_constant_column=true) () =
  { add_constant_column ; l2_regularizer }

let default = opt ()

let pad_design_matrix pred pad =
  let orig     = Mat.of_array pred in
  let num_obs  = Mat.dim1 orig in       (* rows *)
  let num_pred = Mat.dim2 orig in       (* cols *)
  let across_pred_col f =
    (* + 1 since fortran style *)
    Array.init num_pred (fun i -> f (Mat.col orig (i + 1)))
  in
  if pad then
    let fill = lacpy ~bc:2 orig in
    let _    = Mat.fill ~ac:1 ~n:1 fill 1.0 in
    `Padded (orig, fill), num_obs, num_pred + 1, across_pred_col
  else
    `Unpadded orig, num_obs, num_pred, across_pred_col

(* Etc:
  - Should I call:
    - pred the design matrix
    - lambda the ridge parameter
  TODO:
  - when SVD exposes the dimensionality reduction,
    we can add back removed_predictors logic.
  - work through these covariance matrix calculations, they're probably not right
  - once that's done we can expose the hypothesis testing
*)
let regress ?(opt=default) pred ~resp =
  let resp = Vec.of_array resp in
  let pad = opt.add_constant_column in
  let pred, num_obs, num_pred, across_pred_col = pad_design_matrix pred pad in
  (* TODO: replace with folds, across the matrix in Lacaml. *)
  let num_obs_float = float num_obs in
  let m_resp        = col_mean num_obs_float resp in
  let s_yy          = sum_sq_dm resp m_resp in
  let m_pred        = across_pred_col (col_mean num_obs_float) in
  (* since num_pred includes a value for the constant coefficient, no -1 is needed. *)
  let deg_of_freedom  = float (num_obs - num_pred) in
  let lambda          = opt.l2_regularizer in
  let solved_lp       = solve_lp pred resp lambda in
  let sum_residuals   = dot solved_lp.resi solved_lp.resi in
  let inferred_var  = sum_residuals /. deg_of_freedom in
  let aic =
    let n = float num_obs in
    let k = float num_pred in
    2.0 *. k +. (log (sum_residuals /. n)) +. (n +. k) /. (n -. k -. 2.0)
  in
  { padded = pad
  ; m_pred
  ; m_resp
  ; deg_of_freedom
  ; sum_residuals
  ; inferred_var
  ; s_yy
  ; solved_lp
  (*d_w = durbin_watson residuals *)
  ; aic = aic
  }
