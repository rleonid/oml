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

(* - The h estimation logic comes from
   http://en.wikipedia.org/wiki/Numerical_differentiation.
   - The trick to create dx2 tries to ensure that dx2 is always a numerically
     representable number, x_i will be rounded to nearest number.
   *)
let secant, second_order =
  let h x = (max 1.0 (abs_float x)) *. sqrt Util.dx in
  (fun f x ->
    let x_i = x +. h x in
    let dx2 = x_i -. x in
    (f (x +. dx2) -. f x) /. dx2),
  (fun f x ->
    let x_i = x +. h x and x_j = x -. h x in
    let h_1 = x_i -. x and h_2 = x -. x_j in
    (f (x +. h_1) -. f (x -. h_2)) /. (h_1 +. h_2))

