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

(** Methods to estimate derivatives of functions. *)

(** [secant f] estimates the derivative of [f] by evaluating a the slope of a
    secant between the desired point and one close by
    (ie. [(x,f(x))] and [(x+h,f(x+h))]). Errors are [O(h)]. *)
val secant : (float -> float) -> (float -> float)

(** [second_order f] estimates the derivatrive of [f] by evaluating the line
    connecting two points around [x], specifically
    [(x-h,f(x-h))] and [(x+h,f(x+h))]. Errors are [O(h^2)]. *)
val second_order : (float -> float) -> (float -> float)

