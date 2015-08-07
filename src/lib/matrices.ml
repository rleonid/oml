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

open Util

type t = Vectors.t array

let row m i = Array.copy (Array.get m i)

let column m i = Array.init (Array.length m) (fun j -> m.(j).(i))

let dim m = Array.length m, Array.length m.(0)

let transpose m =
  let rows,cols = dim m in
  Array.init cols (fun c ->
    Array.init rows (fun r ->
      m.(r).(c)))

let diagonal v =
  let n = Array.length v in
  Array.init n (fun r ->
    Array.init n (fun c ->
      if r = c then v.(r) else 0.0))

let equal ?d x y =
  let n = Array.length x
  and m = Array.length y in
  let rec loop i =
    i = n || Vectors.equal ?d x.(i) y.(i) && loop (i + 1)
  in
  n = m && loop 0

let add x y = Array.init (Array.length x) (fun i -> Vectors.add x.(i) y.(i))
let sub x y = Array.init (Array.length x) (fun i -> Vectors.sub x.(i) y.(i))

let mult x = Array.map (Vectors.mult x)


let identity n =
  Array.init n (fun i ->
    Array.init n (fun j ->
      if i = j then 1.0 else 0.0))

let prod ml mr =
  let row_l, col_l = dim ml
  and row_r, col_r = dim mr in
  if col_l <> row_r then
    invalidArg "incompatible matrices dimensions (%d, %d)*(%d, %d) for product"
      row_l col_l row_r col_r
  else
    let n = col_l - 1 in
    let s = ref 0.0 in
    Array.init row_l (fun r ->
      Array.init col_r (fun c ->
        s := 0.0;
        for i = 0 to n do
          s := !s +. ml.(r).(i) *. mr.(i).(c)
        done;
        !s))

let prod_row_vector v m =
  let col_l = Array.length v and row_r, col_r = dim m in
  if col_l <> col_r then
    invalidArg "incompatible vector matrix dimensions [%d]*(%d, %d) for prod_row_vector"
      col_l row_r col_r
  else
    let n = col_l - 1 in
    let s = ref 0.0 in
    Array.init col_l (fun c ->
      s := 0.0;
      for i = 0 to n do
        s := !s +. v.(i) *. m.(i).(c)
      done;
      !s)

let prod_column_vector m v =
  let row_l, col_l = dim m and row_r = Array.length v in
  if col_l <> row_r then
    invalidArg "incompatible vector matrix dimensions (%d, %d)*[%d] for prod_column_vector"
      row_l col_l row_r
  else
    let n = col_l - 1 in
    let s = ref 0.0 in
    Array.init row_l (fun r ->
      s := 0.0;
      for i = 0 to n do
        s := !s +. v.(i) *. m.(r).(i)
      done;
      !s)
