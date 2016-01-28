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

open Util
module P = Printf
module Desc = Descriptive
module Dist = Distributions
open Lacaml.D

module type Linear_model_intf = sig
  include Optional_arg_intf

  type input
  type t

  val describe : t -> string

  val eval : t -> input -> float
  val regress : ?spec:spec -> input array -> resp:float array -> t

  val residuals : t -> float array
  val coefficients : t -> float array

  val residual_standard_error : t -> float
  val coeff_of_determination : t -> float

  val confidence_interval : t -> alpha:float -> input -> float * float
  val prediction_interval : t -> alpha:float -> input -> float * float

  val coefficient_tests : ?null:float -> t -> Inference.test array

end

module Univariate = struct

  type input = float

  type t =
    { m_pred           : float          (* mean of predictor. *)
    ; m_resp           : float          (* mean of response. *)
    ; size             : float          (* number of observations. *)
    ; sum_weights      : float
    ; alpha            : float          (* constant term. *)
    ; beta             : float          (* multiplicative term. *)
    ; residuals        : float array
    ; sum_residuals    : float          (* sum of residuals. *)
    ; inferred_var     : float          (* inferred variance of error. *)
    ; s_yy             : float          (* sum of diff or resp to mean, TSS. *)
    ; s_xx             : float          (* sum of diff of pred to mean. *)
    ; goodness_of_fit  : float option
    }

  let alpha lrm = lrm.alpha

  let beta lrm = lrm.beta

  let describe lrm = P.sprintf "%.6f * x + %.6f" lrm.beta lrm.alpha

  let residuals lm = lm.residuals

  let coefficients lm = [| lm.alpha; lm.beta |]

  let residual_standard_error lm = sqrt lm.inferred_var

  let coeff_of_determination lm = 1.0 -. lm.sum_residuals /. lm.s_yy

  let eval lrm x = lrm.alpha +. lrm.beta *. x

  type spec = float array

  let default = [||]

  let regress ?spec pred ~resp =
    let n = Array.length pred in
    (* Optional spec argument allows us to specify the individual error
       weights on each observation. *)
    let weights =
      match spec with
      | None   -> Array.make n 1.0
      | Some a ->
          let an = Array.length a in
          if an <> n then
            invalidArg "regress: spec length %d <> d predictor size %d" an n
          else
            a
    in
    let w_s = Array.sumf weights in
    let sum2 f a1 a2 =
      Array.fold2 (fun s a1 a2 -> Kahan.update s (f a1 a2)) Kahan.empty a1 a2
      |> Kahan.sum
    in
    let s_x = sum2 ( *. ) pred weights in
    let s_y = sum2 ( *. ) resp weights in
    let m_pred = s_x /. w_s in
    let m_resp = s_y /. w_s in
    let d_x = Array.map (fun x -> x -. m_pred) pred in
    let d_y = Array.map (fun y -> y -. m_resp) resp in
    let d_x_w  = Array.map2 (fun w_i d_i -> w_i *. d_i) weights d_x in
    let d_xx_w = sum2 ( *. ) d_x_w d_x in
    let beta   = (sum2 ( *. ) d_x_w d_y) /. d_xx_w in
    let alpha  = m_resp -. beta *. m_pred in
    let rss = Array.map2 (fun x y -> y -. (alpha +. beta *. x)) pred resp in
    let srs = sum2 (fun w_i r -> w_i *. r *. r) weights rss in
    (* degress of freedom: one for the constant and one for beta *)
    let dgf = float (n - 2) in
    let q =
      match spec with
      | None   -> None
      | Some _ -> Some (Functions.chi_square_greater (truncate dgf) srs)
    in
    { m_pred
    ; m_resp
    ; size = float n
    ; sum_weights = w_s
    ; alpha
    ; beta
    ; residuals = rss
    ; sum_residuals = srs
    ; inferred_var = srs /. dgf
    ; goodness_of_fit = q
    ; s_yy = sum2 ( *. ) d_y d_y
    ; s_xx = d_xx_w
    }

  let confidence_interval, prediction_interval =
    let interval a lrm ~alpha x =
      let dgf = lrm.size -. 2.0 in
      let degrees_of_freedom = truncate dgf in
      let t  = Dist.student_quantile ~degrees_of_freedom (alpha /. 2.0) in
      let b  = (x -. lrm.m_pred) ** 2.0 /. lrm.s_xx in
      let c  = lrm.sum_residuals /. dgf in
      let se = sqrt ((a +. b) *. c) in
      let d  = t *. se in
      let y  = eval lrm x in
      (y -. d), (y +. d)
    in
    (fun lrm -> interval (1.0 /. lrm.size) lrm),
    (fun lrm -> interval ((lrm.size +. 1.0) /. lrm.size) lrm)

  let alpha_test ?(null=0.0) t =
    let alpha_var =
      t.inferred_var *. (1. /. t.sum_weights +.
      t.m_pred *. t.m_pred /. t.s_xx)
    in
    let degrees_of_freedom = truncate t.size - 2 in
    let diff = t.alpha -. null in
    Inference.(t_test Two_sided ~degrees_of_freedom ~diff ~error:(sqrt alpha_var))

  let beta_test ?(null=0.0) t =
    let beta_var = t.inferred_var /. t.s_xx in
    let degrees_of_freedom = truncate t.size - 2 in
    let diff = t.beta -. null in
    Inference.(t_test Two_sided ~degrees_of_freedom ~diff ~error:(sqrt beta_var))

  let coefficient_tests ?null t =
    [| alpha_test ?null t ; beta_test ?null t |]

end

type lambda_spec =
  | Spec of float
  | From of float array

type multivariate_spec =
  { add_constant_column : bool
  ; lambda_spec : lambda_spec option
  }

(* 'Solved' (via SVD) linear problem.  *)
type solved_lp =
  { coef : vec
  ; vaco : [ `Svd of Svd.t | `Cov of mat ]  (* variance-covariance rep *)
  ; resi : vec
  ; looe : vec
  }

module SolveLPViaSvd = struct

  open Lacaml_util

  let to_lambda f g lambda_spec =
    let bestl =
      match lambda_spec with
      | Spec l -> l
      | From arr ->
          let loess = Array.map (fun l -> l, g (f l)) arr in
          Array.sort (fun (_,s1) (_,s2) -> compare s1 s2) loess;
          fst loess.(0)
    in
    bestl, f bestl

  let reg_to_lambda svd resp lambda_spec =
    let looe  = Svd.looe svd resp in
    to_lambda looe Vec.ssqr lambda_spec

  let full_looe cmi pred resi =
    let h  = gemm (gemm pred cmi) ~transb:`T pred in
    let y  = Vec.make (Vec.dim resi) 1.0 in
    axpy (Mat.copy_diag h) ~alpha:(-1.0) y;
    Vec.div resi y

  (* Either figure out the best lambda (aka ridge parameter)
    or solve the system without it. *)
  let solve_lp pred resp = function
    | None          ->
        let p    = match pred with | `Padded (_,fill) -> fill | `Unpadded p -> p in
        (* This is only needed for the residuals later, is there a more efficient way? *)
        let dcp  = lacpy p in
        let svd  = Svd.svd p in
        let coef = Svd.solve_linear svd resp in
        let resi = Vec.sub resp (gemv dcp coef) in
        let looe =
          let y = Vec.make (Vec.dim resp) 1.0 in
          axpy (Svd.h_diag svd) ~alpha:(-1.0) y;
          Vec.div resi y
        in
        { coef
        ; vaco = `Svd svd
        ; resi
        ; looe
        }
    | Some lambda_spec ->
        match pred with
        | `Unpadded p ->
            let dcp = lacpy p in
            let svd = Svd.svd p in
            (* Odd, that we should know the error on the coefficients
              _before_ computing them! There's probably a better way to
              structure this! *)
            let lambda, looe = reg_to_lambda svd resp lambda_spec in
            let coef = Svd.solve_linear ~lambda svd resp in
            { coef
            ; vaco = `Svd svd
            ; resi = Vec.sub resp (gemv dcp coef)
            ; looe
            }
        | `Padded (orig, fill) ->
            let svd = Svd.svd orig in
            let lambda, _ = reg_to_lambda svd resp lambda_spec in
            let coef = Svd.solve_linear ~lambda svd resp in
            (* Set the constant term beta to the mean of the response.
              See "Estimation of the constant term when using ridge regression"
              by Bertie and Cran for a clear explanation.  *)
            let mres = col_mean (float (Vec.dim resp)) resp in
            let coef = copy ~ofsy:2 ~y:(Vec.make (Vec.dim coef + 1) mres) coef in
            let resi = Vec.sub resp (gemv fill coef) in
            let looe =
              let cmi = gemm ~transa:`T fill fill in
              getri cmi;
              full_looe cmi fill resi
            in
            { coef ; vaco = `Svd svd ; resi ; looe }

end

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

module EvalMultiVarite = struct

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
        invalidArg "Improper array length: %d exp %d - 1." vn n
      else
        let c = Array.sub coe 1 (n - 1) in
        coe.(0) +. dot c vec
    else if vn <> n then
      invalidArg "Improper array length: %d exp %d." vn n
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
      let t  = Distributions.student_quantile ~degrees_of_freedom (alpha /. 2.0) in
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
      Inference.(t_test Two_sided ~degrees_of_freedom ~diff ~error))

end

module Multivariate = struct

  include EvalMultiVarite

  type spec = multivariate_spec

  let default =
    { add_constant_column = false
    ; lambda_spec = None
    }

  open Lacaml_util
  open SolveLPViaSvd

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
  let regress ?(spec=default) pred ~resp =
    let resp = Vec.of_array resp in
    let pad = spec.add_constant_column in
    let pred, num_obs, num_pred, across_pred_col = pad_design_matrix pred pad in
    (* TODO: replace with folds, across the matrix in Lacaml. *)
    let num_obs_float = float num_obs in
    let m_resp        = col_mean num_obs_float resp in
    let s_yy          = sum_sq_dm resp m_resp in
    let m_pred        = across_pred_col (col_mean num_obs_float) in
    (* since num_pred includes a value for the constant coefficient, no -1 is needed. *)
    let deg_of_freedom  = float (num_obs - num_pred) in
    let lambda          = spec.lambda_spec in
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

end

type tikhonov_spec =
  { regularizer : float array array
  ; lambda_spec : lambda_spec option (* multipliers on the regularizing matrix. *)
  }

module Tikhonov = struct

  include EvalMultiVarite

  type spec = tikhonov_spec

  let default =
    { regularizer = [|[||]|]
    ; lambda_spec = None
    }

  open Lacaml_util
  open SolveLPViaSvd

  let gtr_to_lambda fit_model lambda_spec =
    let g slp = Vec.ssqr slp.looe in
    to_lambda fit_model g lambda_spec

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
    | Some lambda_spec ->
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
        let lambda, slp = gtr_to_lambda eval lambda_spec in
        let _ = P.printf "chose gtr lambda of %0.4f\n" lambda in
        slp

  let regress ?(spec=default) pred ~resp =
    let pred = Mat.of_array pred in
    let resp = Vec.of_array resp in
    (* UGH, awkard! to silence 41, need to 'modularize' these *)
    let lambda = (spec:tikhonov_spec).lambda_spec in
    let tik =
      match spec.regularizer with
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

end

