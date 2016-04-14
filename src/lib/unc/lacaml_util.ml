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

let row_means a =
  let m = Mat.dim2 a in
  gemv ~alpha:(1./.(float m)) a (Vec.make m 1.)

let centered a =
  let u = column_means a in
  let n = Mat.dim1 a in
  ger ~alpha:(-1.) (Vec.make n 1.) u (lacpy a)

(* Create a centering matrix of size [n]. *)
let centering n =
  let i = Mat.identity n in
  let o = Vec.make n 1. in
  ger ~alpha:(-1./. (float n)) o o i

let column_scatter ?(alpha=1.) a =
  let n = float (Mat.dim1 a) in
  let u = column_means a in
  let ralpha = -1. *. n *. alpha in
  let malpha = alpha in
  (* TODO:syrk ? *)
  ger ~alpha:ralpha u u (gemm ~alpha:malpha ~transa:`T a a)

let row_scatter ?(alpha=1.) a =
  let m = float (Mat.dim2 a) in
  let u = row_means a in
  let ralpha = -1. *. m *. alpha in
  let malpha = 1. in
  (* TODO:syrk ? *)
  ger ~alpha:ralpha u u (gemm ~alpha:malpha ~transb:`T a a)

let scale_or_unbiased_rows a = function
  | None   -> 1. /. float (Mat.dim1 a)
  | Some s -> s

let sample_covariance ?scale a =
  let alpha = scale_or_unbiased_rows a scale in
  column_scatter ~alpha a

(* Faster only upper tri form *)
let sample_covariance_upper_tri ?scale a =
  let c = centered a in
  let alpha = scale_or_unbiased_rows a scale in
  syrk ~alpha ~trans:`T c

let msk_mean_mat mask n a =
  let m = Mat.dim2 a in
  Vec.init m (fun i -> msk_mean mask n (Mat.col a i))

type class_positions =
  { mask : Vec.t
  ; inds : int list
  ; size : float
  }

type 'a class_mask = ('a * class_positions) list

let masks cm = List.map (fun (_, {mask;_}) -> mask) cm
let sizes cm = List.map (fun (_, {size;_}) -> size) cm

let class_masks ?class_order cls =
  let rows = List.length cls in
  let htbl = Hashtbl.create (rows / 10) in
  List.iteri (fun i c ->
      let i = i + 1 in
      let nv, nl =
        let v, l =
          match Hashtbl.find htbl c with
          | p -> p
          | exception Not_found -> let v = Vec.make0 rows in v, []
        in
        v.{i} <- 1.0;
        v, (i::l) (* Store these a 1 based indices *)
      in
      Hashtbl.replace htbl c (nv, nl)) cls;
  match class_order with
  | None ->
      Hashtbl.fold (fun c (mask,inds) acc ->
        let size = Vec.sum mask in
        (c, { mask; inds; size}) :: acc) htbl []
  | Some order ->
      List.fold_right (fun c acc ->
        let (mask, inds) = Hashtbl.find htbl c in
        let size = Vec.sum mask in
        (c, { mask; inds; size}) :: acc) order []

let class_means_pip data m f =
  let cc = Mat.of_diag (Vec.reci (Vec.of_list (sizes m))) in
  let mm = Mat.of_col_vecs_list (masks m) in
  let mn = gemm ~transa:`T data (gemm mm cc) in
  f cc mm mn

let class_means data m =
  class_means_pip data m (fun _ _ mn -> mn)

let centered_class a m =
  let mm = Mat.of_col_vecs_list (masks m) in
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
  let v = Vec.sqrt (Vec.of_list (sizes m)) in
  let d = Mat.sub cm (Mat.of_col_vecs_list (List.map (fun _ -> cu) m)) in
  Mat.scal_cols d v;
  gemm d ~transb:`T d

 let determinant_of_one_and_two_d_blocks_fold ~dt1 ipiv n ~f ~init =
  let dt2 i =
    let im1 = i - 1 in                      (* 2D determinant *)
    dt1 i i *. dt1 im1 im1 -. dt1 im1 i *. dt1 im1 i
  in
  let d = ref init in
  for i = 1 to n do
    if ipiv.{i} > 0l then
      d := f !d (dt1 i i)
    else if i > 1 && ipiv.{i} < 0l && ipiv.{i} = ipiv.{i-1} then
      d := f !d (dt2 i)
  done;
  !d

let determinant_of_one_and_two_d_blocks ~dt1 ipiv n =
  determinant_of_one_and_two_d_blocks_fold ~dt1 ipiv n ~f:( *. ) ~init:1.

(* source: http://www.r-bloggers.com/matrix-determinant-with-the-lapack-routine-dspsv/
  TODO: Lacaml's spsv swallowed up the info arg, this needs to be checked.
*)
let determinant_symmetric a =
  let p = Mat.packed a in
  let b = Vec.random (Mat.dim1 a) in
  let n = truncate (sqrt (float (2 * Vec.dim p))) in
  let ipiv = Lacaml.Common.create_int32_vec n in
  (* Solves the linear equation A x = b, where a is upper triangular, by
     factoring A intu U D U^T, where U is unitriangular (1's along main
     diagonal, ie det(U) = 1, and D has either 1x1 or 2x2 matrices along
     it's diagonal. D is returned, also packed in A.
     p = packed A.
    details: http://www.netlib.no/netlib/lapack/double/dspsv.f
  *)
  spsv p (Mat.from_col_vec b) ~ipiv;
  let dt1 i j = p.{i + ((j-1) * j) / 2} in  (* upper triangle packed *)
  determinant_of_one_and_two_d_blocks ~dt1 ipiv n

let determinant_symmetric_and_inverse ?(copy=true) a =
  let p = if copy then lacpy a else a in
  let n = Mat.dim1 a in
  let b = Mat.identity n in
  let ipiv = Lacaml.Common.create_int32_vec n in
  sysv ~ipiv p b;
  let dt1 i j = p.{i,j} in
  determinant_of_one_and_two_d_blocks ~dt1 ipiv n, b

(* Decompose A = into P*L*U, where
   P is a permutation matrix -> det(P) = -1 ** # swaps,
   L is lower uni[triangular/trapezoidal] -> det(L) = 1.
   U is upper triangular/trapezoidal -> det(U) = product of diagonal.

   TODO:
    - make private
    - same tidbit about info, as above, applies.
    - when to use this vs the symmetric approach, experimenting with known
      matrices gives better results with this (LU decomposition) approach.
*)
let det_pipeline ?(copy=true) a f =
  let c = if copy then lacpy a else a in
  (* details: http://www.netlib.no/netlib/lapack/double/dgetrf.f,
     remember that p is not a permutation matrix but just what gets
     swapped, so everytime p.{i} doesn't equal i there is a swap. *)
  let p = getrf c in
  let d = ref 1.0 in
  let n = min (Mat.dim1 a) (Mat.dim2 a) in
  let x = ref 0 in
  for i = 1 to n do
    d := !d *. c.{i,i};
    if p.{i} <> Int32.of_int i then incr x
  done;
  let d = if !x mod 2 = 0 then !d else !d *. -1. (* d *. -1 ** x *) in
  f d p c

let determinant ?copy a =
  det_pipeline ?copy a (fun d _ipiv _c -> d)

let determinant_and_inverse ?copy a =
  det_pipeline ?copy a (fun d ipiv c ->
    getri ~ipiv c;
    d, c)

(* TODO: how to 'scale' the covariance matrices
   should we even allow it?  ... *)
let class_sample_covariance data msk =
  let dt = Mat.transpose_copy data in
  List.map (fun (c, m) ->
    let cov =
      Array.of_list m.inds
      |> Array.map (Mat.col dt)
      |> Mat.of_col_vecs
      |> (fun a -> row_scatter ~alpha:(1. /. float (Mat.dim2 a)) a)
    in
    c, cov) msk
