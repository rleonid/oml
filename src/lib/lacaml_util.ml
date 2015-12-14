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

let scale_by_col_sum a =
  for j = 1 to Mat.dim2 a do
    let c = Mat.col a j in
    ignore (Vec.exp ~y:c c);
    let s = 1. /. Vec.sum c in
    scal s c
  done


