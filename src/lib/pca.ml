
open Lacaml.D

open Lacaml_stats

type t = { variances   : vec
         ; components  : mat
         ; scalings    : (float * float) array
         }

let variances t  = Vec.to_array t.variances
let components t = Mat.to_array t.components
let scalings t   = t.scalings

(* An observation per row with each feature in a column. *)
let pca ?(demean=true) ?(scale=true) ?(unbiased=true) data =
  let adj, wm = normalize ~demean ~scale ~unbiased data in
  let n  = Mat.dim1 data in
  let s  = 1.0 /. float (if unbiased then (n - 1) else n) in
  Mat.scal s wm;
  let s, _, vt = gesvd ~jobu:`S ~jobvt:`S wm in
  { variances   = Vec.sqr s
  ; components  = Mat.transpose vt
  ; scalings    = adj
  }

(* how to reduce the components. *)
type pca_reduction_method =
    | Num_comp of int
    | Varience_exp of float

let reduce t = function
  | Num_comp n ->
      { t with variances = copy ~n t.variances; components = lacpy ~n t.components }
  | Varience_exp th ->
      let totalv = Vec.sum t.variances in
      let (_, n) =
        Vec.fold (fun (s,i as ss) v ->
          if s > th then ss
          else (s +. v /. totalv, i + 1))
          (0.0,0) t.variances
      in
      { t with variances = copy ~n t.variances; components = lacpy ~n t.components }
