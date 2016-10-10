(*
   Copyright 2015,2016:
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

type iterative_failure_reason =
    | Out_of_bounds of float
    | No_convergence
    | Too_many_iterations of int
    | Too_few_iterations of int

exception Iteration_failure of string * iterative_failure_reason

let invalid_arg ?m ?f fmt =
  let p =
    match m, f with
    | None, None          -> ""
    | (Some m), None      -> m ^ ": "
    | None, (Some f)      -> f ^ ": "
    | (Some m), (Some f)  -> Printf.sprintf "%s.%s:" m f
  in
  Printf.ksprintf (fun s -> raise (Invalid_argument (p ^ s))) fmt

let pi = 4. *. atan 1.

let midpoint x y = (x +. y) /. 2.0

(* This value is taken from http://en.wikipedia.org/wiki/Machine_epsilon which
   sites Higham, Nicholas (2002). Accuracy and Stability of Numerical Algorithms
   (2 ed). SIAM. p. 37. *)
let dx = 2.22044e-16

let significantly_different_from ?(d=dx) x y = y < (x -. d) || y > (x +. d)

let equal_floats ~d x y = not (significantly_different_from ~d x y)

let is_nan x = x <> x

let is_degenerate x = is_nan x || x = neg_infinity || x = infinity

type 'a bound = Open of 'a
              | Closed of 'a

let within bp x =
  match bp with
  | (Open l), (Open u)      -> l < x  && x < u
  | (Open l), (Closed u)    -> l < x  && x <= u
  | (Closed l), (Open u)    -> l <= x && x < u
  | (Closed l), (Closed u)  -> l <= x && x <= u

let fst3 (x,_,_) = x
let snd3 (_,x,_) = x
let thr3 (_,_,x) = x

