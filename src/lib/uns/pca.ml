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

open Lacaml.D

open Uncategorized.Lacaml_util

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
  ; components  = Mat.transpose_copy vt
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
