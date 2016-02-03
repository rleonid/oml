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

(** Numerically find the roots of passed functions. *)

(** [newton_raphson_full ?init accuracy iterations lower upper f df]
    uses the Newton Raphson method to iteratively solve [f].

    Starting at mid point of [lower] and [upper], the method
    uses [df] to follow the tangent to it's root, that becomes the new point.
    Iteration continues until [f x] < [accuracy].

    @raise Util.IterationFailure if we escape our bounds or take longer than
      [iterations].
*)
val newton_raphson_full :
  ?init:float -> accuracy:float -> iterations:int -> lower:float ->
    upper:float -> f:(float -> float) -> df:(float -> float) -> float

(** [newton ?init lower upper f] is equivalent to
    [newton_raphson_full ?init ~accuracy:1.0e-10 ~iterations:1000
      lower upper f (Estimations.second_order f)] *)
val newton : ?init:float -> lower:float ->
            upper:float -> (float -> float) -> float

(** [bisection epsilon lower upper f] iteratively finds the root of [f] between
    [lower] and [upper] using the bisection method.

    @raise Invalid_arg If the function does not take opposite signs at the
      supplied bounds.  *)
val bisection : epsilon:float -> lower:float -> upper:float ->
    (float -> float) -> float

(*val debug_ref : bool ref *)
