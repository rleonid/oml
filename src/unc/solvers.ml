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

open Oml_util
let invalid_arg ~f fmt = invalid_arg ~m:"Solvers" ~f fmt

let newton_raphson_full ?init ~accuracy ~iterations ~lower ~upper ~f ~df =
  let rec loop x i =
(*  printf "i %d: x %f\n" i x; *)
    if x < lower then
      raise (Iteration_failure ("newton raphson", Out_of_bounds lower))
    else if x > upper then
      raise (Iteration_failure ("newton raphson", Out_of_bounds upper))
    else if i > iterations then
      raise (Iteration_failure ("newton raphson", Too_many_iterations i))
    else
      let y = f x in
      if abs_float y < accuracy then
        x
      else
        let x_n = x -. (y /. (df x)) in
        loop x_n (i + 1)
  in
  match init with
  | None   -> loop (midpoint upper lower) 0
  | Some i -> loop i 0

let newton ?init ~lower ~upper f =
  newton_raphson_full ?init ~accuracy:1.0e-10 ~iterations:1000
      ~lower ~upper ~f ~df:(Estimations.second_order f)

let debug_ref = ref false

let bisection_explicit ~epsilon ~lower ~upper f =
  let eq_zero v = not (significantly_different_from ~d:epsilon 0.0 v) in
  let root_inside l u = (l < 0.0 && u > 0.0) || (l > 0.0 && u < 0.0) in
  let rec loop lb ub =
    let mp = midpoint lb ub in
    if (abs_float (ub -. lb)) < 2.0 *. epsilon ||
       (* use exact comparison to detect when rounding is dropping precision*)
       mp = lb || mp = ub then
      `CloseEnough mp
    else
      let flb = f lb in
      let fub = f ub in
      if !debug_ref then
        Printf.printf "lb %.16f ub %.16f flb %.16f fub %.16f \n%!" lb ub flb fub;
      (* Are we lucky? *)
      if eq_zero flb then
        `EqZero lb
      else if eq_zero fub then
        `EqZero ub
      (* if the user does not provide an interval
          where the function has an intersection. *)
      else if flb < 0.0 && fub < 0.0 then
        `Outside ub
      else if flb > 0.0 && fub > 0.0 then
        `Outside lb
      else
        let fmp = f mp in
        if eq_zero fmp then
          `EqZero mp
        else if root_inside flb fmp then (* go left! *)
          loop lb mp
        else if root_inside fmp fub then (* go right! *)
          loop mp ub
        else
          `IntermediateValueTheoremViolated mp
  in
  loop lower upper

let bisection ~epsilon ~lower ~upper f =
  match bisection_explicit ~epsilon ~lower ~upper f with
  | `EqZero v
  | `CloseEnough v -> v
  | `Outside _     ->
      invalid_arg ~f:"bisection" "Function does not take oppositely signed values at bounds"
  | `IntermediateValueTheoremViolated v ->
      invalid_arg ~f:"bisection" "Intermediate Value Theorem has been violated: %f" v
