open Lacaml_util
open Lacaml.D
module Svd = Svd

(* 'Solved' (via SVD) linear problem. *)
type solved_lp =
  { coef : vec
  ; vaco : [ `Svd of Svd.t | `Cov of mat ]  (* variance-covariance rep *)
  ; resi : vec
  ; looe : vec
  }

let to_lambda f g l2_regularizer =
  let bestl =
    match l2_regularizer with
    | `S l -> l
    | `From arr ->
        let loess = Array.map (fun l -> l, g (f l)) arr in
        Array.sort (fun (_,s1) (_,s2) -> compare s1 s2) loess;
        fst loess.(0)
  in
  bestl, f bestl

let reg_to_lambda svd resp l2_regularizer =
  let looe  = Svd.looe svd resp in
  to_lambda looe Vec.ssqr l2_regularizer

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
  | Some l2_regularizer ->
      match pred with
      | `Unpadded p ->
          let dcp = lacpy p in
          let svd = Svd.svd p in
          (* Odd, that we should know the error on the coefficients
            _before_ computing them! There's probably a better way to
            structure this! *)
          let lambda, looe = reg_to_lambda svd resp l2_regularizer in
          let coef = Svd.solve_linear ~lambda svd resp in
          { coef
          ; vaco = `Svd svd
          ; resi = Vec.sub resp (gemv dcp coef)
          ; looe
          }
      | `Padded (orig, fill) ->
          let svd = Svd.svd orig in
          let lambda, _ = reg_to_lambda svd resp l2_regularizer in
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

