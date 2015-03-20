
open Util


let standard_normal_cdf z =
  try (1.0 +. Functions.erf (z /. sqrt 2.0)) /. 2.0
  with e ->
    Printf.eprintf "standard_normal_cdf failed for %f\n" z;
    raise e

let normal_cdf ~mean ~std x =
  let z = ((x -. mean) /. std) in
  standard_normal_cdf z

let normal_pdf ~mean ~std x =
  let z = ((x -. mean) /. std) in
  (exp ((-1.0 /. 2.0) *. (z ** 2.0))) /.  (std *. (sqrt (2.0 *. pi)))

let standard_normal_pdf =
  normal_pdf ~mean:0.0 ~std:1.0

let poisson_cdf ~mean k =
    Functions.gammaq (floor (k +. 1.0)) mean
