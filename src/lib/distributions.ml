
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
    Functions.regularized_upper_gamma (floor (k +. 1.0)) mean

let ln_beta_pdf ~alpha ~beta =
  if alpha <= 0.0 then raise (Invalid_argument "alpha") else
    if beta <= 0.0 then raise (Invalid_argument "beta") else
      let alpha_minus_1 = alpha -. 1.0 in
      let beta_minus_1 = beta -. 1.0 in
      let z = Functions.ln_beta alpha beta in fun x ->
        if x < 0.0 || x > 1.0 then neg_infinity else
          if x = 0.0 then
            (if (alpha < 1.0) then raise (Invalid_argument "alpha") else neg_infinity)
            else if x = 1.0 then (if (beta < 1.0) then raise (Invalid_argument "beta")
          else neg_infinity) else
            let logX = log x in
            let log1_minus_x = log1p (-.x) in
            alpha_minus_1 *. logX +. beta_minus_1 *. log1_minus_x -. z

let beta_pdf ~alpha ~beta =
  let ln_pdf = ln_beta_pdf ~alpha ~beta in
  fun x -> let value = ln_pdf x in
  if value = neg_infinity then 0.0 else exp value

let beta_cdf ~alpha ~beta =
  if alpha <= 0.0 then raise (Invalid_argument "alpha") else
    if beta <= 0.0 then raise (Invalid_argument "beta") else
      let reg = Functions.regularized_beta ~alpha ~beta in
      let z = Functions.beta alpha beta in
      fun x -> if x <= 0.0 then 0.0 else if x >= 1.0 then 1.0 else
        (reg x) /. z
