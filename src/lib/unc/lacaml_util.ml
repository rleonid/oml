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

(* column mean. *)
let col_mean n c = Vec.sum c /. n
let msk_mean mask n c = (dot mask c) /. n

(* sum of squares diff of col *)
let sum_sq_dm c m = Vec.ssqr (Vec.add_const (-.m) c)

(* column standard deviation *)
let col_std c m n = sqrt (sum_sq_dm c m /. n)

let normalize ?(demean=true) ?(scale=true) ?(unbiased=true) m =
  let r = Mat.dim1 m in
  let n = float r in
  let f col =
    let m = if demean then col_mean n col else 0.0 in
    let s =
      if scale then
        col_std col m (if unbiased then n -. 1.0 else n)
        else 1.0
    in
    let dm  = Vec.make r (-.m/.s) in
    axpy ~alpha:(1.0/.s) col dm;
    ((m,s),dm)
  in
  let adj_cols_arr =
    Mat.to_col_vecs m
    |> Array.map f
  in
  let adj = Array.map fst adj_cols_arr in
  let mmm =
    Array.map snd adj_cols_arr
    |> Mat.of_col_vecs
  in
  adj, mmm

(* TODO/Observations:
  1. This work can/should be parameterized to be independent of row/column
     feature/data orientation. Right now each row represents a data point so
     columns are treated as features.
  2. Many of the resulting matrix calculations are symmetric, need to
     parameterize by what the resulting operation needs. *)

let remove_column_max a =
  let one = Vec.make (Mat.dim1 a) 1. in
  for j = 1 to Mat.dim2 a do
    let c = Mat.col a j in
    let mx = Vec.max c in
    axpy ~alpha:(-1. *. mx) one c
  done

let column_means a =
  let n = Mat.dim1 a in
  gemv ~alpha:(1./. (float n)) ~trans:`T a (Vec.make n 1.)

let centered a =
  let u = column_means a in
  let n = Mat.dim1 a in
  ger ~alpha:(-1.) (Vec.make n 1.) u (lacpy a)

(* Create a centering matrix of size [n]. *)
let centering n =
  let i = Mat.identity n in
  let o = Vec.make n 1. in
  ger ~alpha:(-1./. (float n)) o o i

let scatter ?(alpha=1.) a =
  let n = float (Mat.dim1 a) in
  let u = column_means a in
  let ralpha = -1. *. n *. alpha in
  let malpha = alpha in
  ger ~alpha:ralpha u u (gemm ~alpha:malpha ~transa:`T a a)

let scale_or_unbiased_rows a = function
  | None ->
    let n = Mat.dim1 a in
    1. /. float n
  | Some s -> s

let sample_covariance ?scale a =
  let alpha = scale_or_unbiased_rows a scale in
  scatter ~alpha a

(* Faster only upper tri form *)
let sample_covariance_upper_tri ?scale a =
  let c = centered a in
  let alpha = scale_or_unbiased_rows a scale in
  syrk ~alpha ~trans:`T c

let msk_mean_mat mask n a =
  let m = Mat.dim2 a in
  Vec.init m (fun i -> msk_mean mask n (Mat.col a i))

type 'a class_mask =
  { order : 'a list
  ; masks : Vec.t list
  ; sizes : float list
  }

let class_masks cls =
  let rows = List.length cls in
  let htbl = Hashtbl.create (rows / 10) in
  List.iteri (fun i c ->
      match Hashtbl.find htbl c with
      | v -> v.{i + 1} <- 1.0
      | exception Not_found ->
          let v = Vec.make0 rows in
          v.{i + 1} <- 1.0;
          Hashtbl.add htbl c v) cls;
  let order, masks, sizes =
    Hashtbl.fold (fun c v (c_a,v_a,n_a) ->
      let n = Vec.sum v in
      (c::c_a,v::v_a,n::n_a))
       htbl ([],[],[])
  in
  {order; masks; sizes}

let class_means data m =
  let cc = Mat.of_diag (Vec.reci (Vec.of_list m.sizes)) in
  let mm = Mat.of_col_vecs_list m.masks in
  gemm ~transa:`T data (gemm mm cc)

let centered_class a m =
  let mm = Mat.of_col_vecs_list m.masks in
  let mu = class_means a m in
  Mat.sub a (gemm mm ~transb:`T mu)

let within_class_scatter a m =
  let c = centered_class a m in
  gemm ~transa:`T c c

(* Faster only upper tri form *)
let within_class_scatter_upper_tri a m =
  let c = centered_class a m in
  syrk ~trans:`T c

let between_class_scatter a m =
  let cu = column_means a in
  let cm = class_means a m in
  let v = Vec.sqrt (Vec.of_list m.sizes) in
  let d = Mat.sub cm (Mat.of_col_vecs_list (List.map (fun _ -> cu) m.sizes)) in
  Mat.scal_cols d v;
  gemm d ~transb:`T d
