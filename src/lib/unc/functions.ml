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

(* TODO: Figure out the best bound for this and ensure the more
  accurate method is used. I think that Cephes might already do this
  but I need to make sure. *)
let erf = Ocephes.erf
let erfc = Ocephes.erfc

let gamma = Ocephes.gamma
let ln_gamma = Ocephes.lgam

let regularized_lower_gamma = Ocephes.igam
let regularized_upper_gamma = Ocephes.igamc

let ln_beta x y = ln_gamma x +. ln_gamma y -. ln_gamma (x +. y)

let ln_multivariate_beta arr =
  (Array.sumf (Array.map ln_gamma arr)) -.
  ln_gamma (Array.sumf arr)

let beta x y = exp (ln_beta x y)

let multivariate_beta arr = exp (ln_multivariate_beta arr)

let rec regularized_beta ~alpha:a ~beta:b ?epsilon ?max_iterations =
  let get_b n x =
    if (n mod 2 = 0) then
      let m = float n /. 2.0 in
      (m *. (b -. m) *. x) /.
                            ((a +. (2. *. m) -. 1.) *. (a +. (2. *. m)))
  else
    let m = (float n -. 1.0) /. 2.0 in
    -.((a +. m) *. (a +. b +. m) *. x) /.
                                ((a +. (2. *. m)) *. (a +. (2. *. m) +. 1.0)) in
  let get_a _n _x = 1.0 in
  let log_beta = ln_beta a b in
  let fraction = Continued_fraction.init ~get_a ~get_b in fun x ->
    if Util.is_nan x || Util.is_nan a || Util.is_nan b ||
            x < 0.0 || x > 1.0 || a <= 0.0 || b <= 0.0 then nan
    else if (x > (a +. 1.) /. (2. +. b +. a) &&
                   1. -. x <= (b +. 1.) /. (2. +. b +. a))
    then 1. -. regularized_beta ~alpha:b ~beta:a ?epsilon ?max_iterations (1. -. x)
    else exp ((a *. log x) +. (b *. log1p (-.x)) -.
                log a -. log_beta) *.
                1.0 /. Continued_fraction.evaluate fraction ?epsilon ?max_iterations x

let chi_square_less num_observations chi_square =
  regularized_lower_gamma ~a:((float num_observations) /. 2.0) (chi_square /. 2.0)

let chi_square_greater num_observations chi_square =
  regularized_upper_gamma ~a:((float num_observations) /. 2.0) (chi_square /. 2.0)

let normal_cdf_inv = Ocephes.ndtri

let student_cdf_inv ~degrees_of_freedom = Ocephes.stdtri ~k:degrees_of_freedom

let f_less ~d1 ~d2 x =
  let xr = Float.((d1 * x) / (d1 * x + d2)) in
  regularized_beta ~alpha:(d1 /. 2.) ~beta:(d2 /. 2.) xr
