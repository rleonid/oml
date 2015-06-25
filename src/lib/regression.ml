
open Util
open Inference
open Descriptive

type linear_model = { m_pred                : float
                    ; m_resp                : float
                    ; size                  : float
                    ; alpha                 : float
                    (*; alpha_test            : test *)
                    ; beta                  : float
                    (*; beta_test             : test *)
                    (*; alpha_var             : float *)
                    (*; beta_var              : float *)
                    ; correlation           : float
                    ; chi_square            : float
                    ; inferred_response_var : float
                    ; goodness_of_fit       : float option
                    ; s_xx                  : float
                    (*; d_w                   : float *)
                    }

let to_string lrm = Printf.sprintf "%.6f * x + %.6f" lrm.beta lrm.alpha

(* resp = alpha + beta * pred *)
let eval_lrm lrm x = lrm.alpha +. lrm.beta *. x

let linear_regress ?pred_variance ~resp ~pred () =
  let corr = correlation pred resp in
  let deg_of_freedom = float (Array.length pred - 2) in (* one for the constant and one for beta *)
  let act_pv =
    match pred_variance with
    | None -> Array.init (Array.length pred) (fun _ -> 1.0)
    | Some a -> a
  in
  let s   = Array.sumf (Array.map (fun v -> 1.0 /. v) act_pv) in          (* a funny way of saying n *)
  let s_x = Array.sumf (Array.map2 (fun x v -> x /. v) pred act_pv) in
  let s_y = Array.sumf (Array.map2 (fun y v -> y /. v) resp act_pv) in
  let d   = s_x /. s in
  let t_arr = Array.map2 (fun x_i v_i -> (1.0 /. (sqrt v_i)) *. (x_i -. d)) pred act_pv in
  let s_tt  = Array.sumf (Array.map (fun t_i -> t_i *. t_i) t_arr) in
  let b   = Array.map2 (fun t_i y_i -> t_i *. y_i) t_arr resp
            |> Array.map2 (fun v_i d -> d /. (sqrt v_i)) act_pv
            |> Array.sumf in
  let beta  = b /. s_tt in
  let alpha = (s_y -. (s_x *. beta)) /. s in
  (*let alpha_var = (1.0 +. (s_x *. s_x) /. (s *. s_tt)) /. s in
  let beta_var  = 1.0 /. s_tt in *)
  let residuals   = Array.map2 (fun x y -> y -. alpha -. beta *. x) pred resp in
  let chi_square  = Array.sumf (Array.map2 (fun r v -> (r *. r) /. v) residuals act_pv) in
  (*let rmse      = sqrt (chi_square /. deg_of_freedom) in *)
  let m_x  = mean pred in
  let s_xx = Array.sumf (Array.map (fun x -> (x -. m_x) ** 2.0) pred) in
  (*
  let alpha_test =
      let se   = rmse *. (sqrt alpha_var) in
      let stat = alpha /. se in
      { standard_error     = se;
        degrees_of_freedom = deg_of_freedom;
        stat = stat;
        prob_by_chance = 1.0 -. (student_t_test_sig (abs stat) deg_of_freedom);
      }
  in
  let beta_test =
      let se = rmse * (sqrt beta_var) in
      let stat = beta / se in
      { standard_error = se;
        degrees_of_freedom = deg_of_freedom;
        stat = stat ;
        prob_by_chance = 1.0 - (student_t_test_sig (abs stat) deg_of_freedom);
      }
  in
  *)
  let q =
    match pred_variance with
    | None   -> None
    | Some _ -> Some (Functions.chi_square_greater chi_square (truncate deg_of_freedom))
  in
  (*let n = Array.length residuals in
  let d_w = durbin_watson residuals in *)
  { m_pred = mean pred;
    m_resp = mean resp;
    size = deg_of_freedom +. 2.0;
    alpha = alpha;
    (* alpha_test = alpha_test; *)
    beta = beta;
    (*beta_test = beta_test; *)
    correlation = corr;
    chi_square ;
    inferred_response_var = chi_square /. deg_of_freedom;
    goodness_of_fit = q;
    s_xx = s_xx;
    (*d_w = nan; *)
  }

let confidence_interval, prediction_interval =
  let interval a lrm ~alpha_level x =
    let dgf = lrm.size -. 2.0 in
    let dgi = truncate dgf in
    let t  = Functions.t_lookup (alpha_level /. 2.0) dgi in
    let y  = eval_lrm lrm x in
    let b  = (x -. lrm.m_pred) ** 2.0 /. lrm.s_xx in
    let c  = lrm.chi_square /. (lrm.size -. 2.0) in
    let se = sqrt ((a +. b) *. c) in
    let d  = t *. se in
    (y -. d), (y +. d)
  in
  (fun lrm -> interval (1.0 /. lrm.size) lrm),
  (fun lrm -> interval ((lrm.size +. 1.0) /. lrm.size) lrm)

(* general linear least squares. *)
type general_linear_model = { padded                  : bool
                            ; g_m_pred                : float array
                            ; g_m_resp                : float
                            ; deg_of_freedom          : float
                            ; coefficients            : float array
                            (*; coefficient_tests     : test array *)
                            ; correlations            : float array
                            ; chi_square              : float
                            ; g_inferred_response_var : float
                            ; sum_squares             : float
                            ; cod                     : float
                            ; adj_cod                 : float
                            ; covariance              : float array array
                            ; residuals               : float array
 (*                         ; d_w                     : float
                            Durbin Watson scores. [0..4] with a mean of 2.0 lower
                            ( < 1) indicates positive correlation while
                            higher (> 3) indicates negative correlation. *)
                            ; aic                     : float
                            ; loocv                   : float array
                            }

let eval_glm glm vec =
  if glm.padded then
    let n = Array.length glm.coefficients in
    let c = Array.sub glm.coefficients 1 (n - 1) in
    glm.coefficients.(0) +. Vectors.dot c vec
  else
    Vectors.dot glm.coefficients vec

type lambda_spec =
  [ `Spec of float
  | `From of float array
  | `Within of float * float * float  (* lower bound, upper bound, stopping distance. *)
  ]

open Lacaml.D

let to_lambda svd resp lambda_spec =
  let looe  = Svd.looe svd resp in
  let bestl =
    match lambda_spec with
    | `Spec l               -> l
    | `From arr             ->
        let loess = Array.map (fun l -> l, Vec.ssqr (looe l)) arr in
        (*let _ = Array.iter (fun (v, s) -> Printf.printf "for %0.4f\t %0.4f\n" v s) loess in *)
        Array.sort (fun (_,s1) (_,s2) -> compare s1 s2) loess;
        fst loess.(0)
    | `Within (lb, ub, dl)  ->
        let looe  = Svd.looe svd resp in
        let loess l = Vec.ssqr (looe l) in
        let rec loop lb ub =
          let m = midpoint lb ub in
          if ub -. lb < dl then
            loess m
          else
            let m1 = midpoint lb m in
            let m2 = midpoint m ub in
            let fm1 = loess m1 in
            let fm2 = loess m2 in
            if fm1 < fm2 then
              loop lb m
            else
              loop m ub
        in
        loop lb ub
  in
  bestl, looe bestl

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

(* column mean. *)
let col_mean c    = Vec.sum c /. (float (Vec.dim c))

(* sum of squares diff of col *)
let sum_sq_dm c m = Vec.ssqr (Vec.add_const (-.m) c)

(* column standard deviation *)
let col_std c m n = sqrt (sum_sq_dm c m /. n)

type solved_lp =
  { coef : vec
  ; covm : mat
  ; resi : vec
  ; looe : vec
  }

(* Either figure out the best lambda (aka ridge parameter)
   or solve the system without it. *)
let coefficients_and_covariance pred resp = function
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
      ; covm = Svd.covariance_matrix svd
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
          let lambda, looe = to_lambda svd resp lambda_spec in
          let coef = Svd.solve_linear ~lambda svd resp in
          { coef
          ; covm = Svd.covariance_matrix ~lambda svd
          ; resi = Vec.sub resp (gemv dcp coef)
          ; looe
          }
      | `Padded (orig, fill) ->
          let svd = Svd.svd orig in
          let lambda, _ = to_lambda svd resp lambda_spec in
          let coef = Svd.solve_linear ~lambda svd resp in
          let covm = Svd.covariance_matrix ~lambda svd in
          (* Set the constant term beta to the mean of the response.
            See "Estimation of the constant term when using ridge regression"
            by Bertie and Cran for a clear explanation.  *)
          let mres = col_mean resp in
          let coef = copy ~ofsy:2 ~y:(Vec.make (Vec.dim coef + 1) mres) coef in
          let covm =
            let s = Mat.dim1 covm in
            lacpy ~b:(Mat.make0 (s + 1) (s + 1)) ~br:2 ~bc:2 covm
          in
          let resi = Vec.sub resp (gemv fill coef) in
          let looe =
            let cm = gemm ~transa:`T fill fill in
            getri cm (* take inverse *);
            let h  = gemm (gemm fill cm) ~transb:`T fill in
            Vec.div resi (Mat.copy_diag h)
          in
          { coef ; covm ; resi ; looe }

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
let general_linear_regress ?lambda ?(pad=false) ~resp ~pred () =
  let resp = Vec.of_array resp in
  let pred, num_obs, num_pred, across_pred_col = pad_design_matrix pred pad in
  (* TODO: replace with folds, across the matrix in Lacaml. *)
  let g_m_resp      = col_mean resp in
  let sum_squares   = sum_sq_dm resp g_m_resp in
  let num_obs_float = float num_obs in
  let g_s_resp      = col_std resp g_m_resp num_obs_float in
  let col_corr c    =
    let m = Vec.sum c /. num_obs_float in
    let s = col_std c m num_obs_float in
    let num = (dot c resp) -. num_obs_float *. m *. g_m_resp in
    let den = (num_obs_float -. 1.0) *. g_s_resp *. s in
    num /. den
  in
  let g_m_pred      = across_pred_col col_mean in
  let correlations  = across_pred_col col_corr in
  (* since num_pred includes a value for the constant coefficient, no -1 is needed. *)
  let deg_of_freedom  = float (num_obs - num_pred) in
  let solved_lp       = coefficients_and_covariance pred resp lambda in
  let chi_square      = dot solved_lp.resi solved_lp.resi in
  let infer_resp_var  = chi_square /. deg_of_freedom in
  let m   = (float num_obs -. 1.0) /. deg_of_freedom in
  let aic =
    let n = float num_obs in
    let k = float num_pred in
    2.0 *. k +. (log (chi_square /. n)) +. (n +. k) /. (n -. k -. 2.0)
  in
  { padded = pad
  ; g_m_pred = g_m_pred
  ; g_m_resp = g_m_resp
  ; deg_of_freedom = deg_of_freedom
  ; coefficients = Vec.to_array solved_lp.coef
  ; correlations = correlations
  ; chi_square
  ; g_inferred_response_var = infer_resp_var
  ; sum_squares
  ; cod = 1.0 -. (chi_square /. sum_squares)
  ; adj_cod = 1.0 -. (chi_square /. sum_squares) *. m
  ; covariance = Mat.to_array solved_lp.covm
  ; residuals = Vec.to_array solved_lp.resi
  (*d_w = durbin_watson residuals *)
  ; aic = aic
  ; loocv = Vec.to_array solved_lp.looe
  }

let general_tikhonov_regression ~resp ~pred ~tik =
  let open Lacaml.D in
  let pred = Mat.of_array pred in
  let tik  = Mat.of_array tik in
  let resp = Vec.of_array resp in
  Mat.axpy (gemm ~transa:`T pred pred) tik;
  getri tik;      (* take inverse with LU decomp, tik is overwritten. *)
  let coef = gemv (gemm tik ~transb:`T pred) resp in
  let num_obs  = Mat.dim1 pred in  (* rows *)
  let num_pred = Mat.dim2 pred in  (* cols *)
  (* correlation against the constant column do not make sense,
     is always nan ignore *)
  let across_pred_col f = Array.init num_pred (fun i -> f (Mat.col pred (i + 1))) in
  let col_mean c    = Vec.sum c /. (float (Vec.dim c)) in
  let g_m_resp      = col_mean resp in
  let sum_sq_dm c m = Vec.ssqr (Vec.add_const (-.m) c) in
  let sum_squares   = sum_sq_dm resp g_m_resp in
  let col_std c m n = sqrt (sum_sq_dm c m /. n) in
  let num_obs_float = float num_obs in
  let g_s_resp      = col_std resp g_m_resp num_obs_float in
  let col_corr c    =
    let m = Vec.sum c /. num_obs_float in
    let s = col_std c m num_obs_float in
    let num = (dot c resp) -. num_obs_float *. m *. g_m_resp in
    let den = (num_obs_float -. 1.0) *. g_s_resp *. s in
    num /. den
  in
  let g_m_pred      = across_pred_col col_mean in
  let correlations  = across_pred_col col_corr in
  (* since num_pred includes a value for the constant coefficient, no -1 is needed. *)
  let deg_of_freedom  = float (num_obs - num_pred) in
  let residuals       = Vec.sub resp (gemv pred coef) in
  let chi_square      = dot residuals residuals in
  let infer_resp_var  = chi_square /. deg_of_freedom in
  let m   = (float num_obs -. 1.0) /. deg_of_freedom in
  let aic =
    let n = float num_obs in
    let k = float num_pred in
    2.0 *. k +. (log (chi_square /. n)) +. (n +. k) /. (n -. k -. 2.0)
  in
  { padded = false
  ; g_m_pred = g_m_pred
  ; g_m_resp = g_m_resp
  ; deg_of_freedom
  ; coefficients = Vec.to_array coef
  ; correlations
  ; chi_square
  ; g_inferred_response_var = infer_resp_var
  ; sum_squares
  ; cod = 1.0 -. (chi_square /. sum_squares)
  ; adj_cod = 1.0 -. (chi_square /. sum_squares) *. m
  ; covariance = [||]   (* TODO. *)
  ; residuals = Vec.to_array residuals
  ; aic = aic
  ; loocv = [||]
  }

