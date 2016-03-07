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
let msk_mean m c = (dot m c) /. (Vec.sum m)

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

let remove_column_max a =
  let one = Vec.make (Mat.dim1 a) 1. in
  for j = 1 to Mat.dim2 a do
    let c = Mat.col a j in
    let mx = Vec.max c in
    axpy ~alpha:(-1. *. mx) one c
  done

let column_means a =
  let n = float (Mat.dim1 a) in
  let m = Mat.dim2 a in
  Vec.init m (fun i -> col_mean n (Mat.col a i))

let centered a =
  let u = column_means a in
  let n = Mat.dim1 a in
  ger ~alpha:(-1.) (Vec.make n 1.) u (lacpy a)

let scale_or_unbiased_rows a = function
  | None ->
    let n = Mat.dim1 a in
    1. /. (float (n - 1))
  | Some s -> s

let sample_covariance ?scale a =
  let c = centered a in
  let alpha = scale_or_unbiased_rows a scale in
  gemm ~alpha ~transa:`T c c

(* Faster only upper tri form *)
let sample_covariance_upper_tri ?scale a =
  let c = centered a in
  let alpha = scale_or_unbiased_rows a scale in
  syrk ~alpha ~trans:`T c

(* Slow method
let class_mean_maps data cls =
  let cols = Mat.dim2 data in
  let rows = Mat.dim1 data in
  let htbl = Hashtbl.create (rows / 2) in
  List.iteri (fun i c ->
    match Hashtbl.find htbl c with
    | l -> Hashtbl.replace htbl c (i :: l)
    | exception Not_found ->
      Hashtbl.add htbl c [i]) cls;
  Hashtbl.fold (fun c idxs a ->
      let v = Array.of_list idxs in
      let rows = Array.length v in
      let cms =
        column_means
          (Mat.init_rows rows cols (fun r c -> data.{v.(r-1)+1, c}))
      in
      (c, cms) :: a) htbl [] *)

let msk_mean_mat mask a =
  let m = Mat.dim2 a in
  Vec.init m (fun i -> msk_mean mask (Mat.col a i))

let class_masks data cls =
  let rows = Mat.dim1 data in
  let htbl = Hashtbl.create (rows / 10) in
  List.iteri (fun i c ->
      match Hashtbl.find htbl c with
      | v -> v.{i + 1} <- 1.0
      | exception Not_found ->
          let v = Vec.make0 rows in
          v.{i + 1} <- 1.0;
          Hashtbl.add htbl c v) cls;
  htbl

let class_means data masks =
  Hashtbl.fold (fun c v a ->
    let means = msk_mean_mat v data in
    (c, means) :: a) masks []

let mean_matrix ?alpha ?init data masks =
  let a =
    match init with
    | Some i -> i
    | None -> Mat.make0 (Mat.dim1 data) (Mat.dim2 data)
  in
  Hashtbl.fold (fun _ v a ->
    let means = msk_mean_mat v data in
    ger ?alpha v means a) masks a

let centered_class a masks =
  mean_matrix ~alpha:(-1.) ~init:(lacpy a) a masks

let covariance_class ?scale a masks =
  let c = centered_class a masks in
  let alpha = scale_or_unbiased_rows a scale in
  gemm ~alpha ~transa:`T c c

(* Faster only upper tri form *)
let covariance_class_upper_tri ?scale a =
  let c = centered_class a masks in
  let alpha = scale_or_unbiased_rows a scale in
  syrk ~alpha ~trans:`T c

let between a masks =
  let m = Mat.dim2 a in
  let u = column_means a in
  class_means data masks
  |> List.fold_left (fun a (_, v) ->
       let d = Vec.sub v u in
       ger ~alpha:50. d d a)
      (Mat.make0 4 4)


