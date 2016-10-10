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

open Oml_util
module P = Printf

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
  (*; goodness_of_fit  : float option *)
  }

let alpha lrm = lrm.alpha

let beta lrm = lrm.beta

let describe lrm = P.sprintf "%.6f * x + %.6f" lrm.beta lrm.alpha

let residuals lm = lm.residuals

let coefficients lm = [| lm.alpha; lm.beta |]

let residual_standard_error lm = sqrt lm.inferred_var

let eval lrm x = lrm.alpha +. lrm.beta *. x

let coeff_of_determination lm = 1.0 -. lm.sum_residuals /. lm.s_yy

type opt = float array

let opt ?weights () = match weights with | None -> [||] | Some a -> a

let default = opt ()

let regress ?(opt=default) pred ~resp =
  let n = Array.length pred in
  (* Optional argument allows us to specify the individual error
      weights on each observation. *)
  let an = Array.length opt in
  let weights =
    if an = 0 then Array.make n 1.0
    else if an <> n then
      invalid_arg ~m:"Univariate" ~f:"regress"
        "opt length %d <> d predictor size %d" an n
    else
      opt
  in
  let w_s = Array.sumf weights in
  let sum2 f a1 a2 =
    Array.fold2
      (fun s a1 a2 -> Kahan.update s (f a1 a2)) Kahan.empty a1 a2
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
  (*let q =
    match opt with
    | None   -> None
    | Some _ -> Some (Functions.chi_square_greater (truncate dgf) srs)
  in*)
  { m_pred
  ; m_resp
  ; size = float n
  ; sum_weights = w_s
  ; alpha
  ; beta
  ; residuals = rss
  ; sum_residuals = srs
  ; inferred_var = srs /. dgf
  (*; goodness_of_fit = q *)
  ; s_yy = sum2 ( *. ) d_y d_y
  ; s_xx = d_xx_w
  }

let f_statistic t =
  (t.s_yy -. t.sum_residuals) /. (t.sum_residuals /. (t.size -. 2.))
