
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

