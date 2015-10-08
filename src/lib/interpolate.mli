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

(** [linear (x1,y1) (x2,y2)] fits a line (l) between the two points such that
    given an x value one can get the y along that line with [l x]. *)
val linear : float * float -> float * float -> (float -> float)

(** Cubic splines provide piecewise polynomial to fit the data that is smooth
    at the fit data points (aka knots). This is achieved by requiring that the
    polynomials on the two sides of a knot have the same first and second
    derivative at that point. *)
module Spline : sig

  type boundary_condition =
    | Natural   (** The 2nd derivatives at the end points are 0,
                    [y''(x_0) = y''(x_n) = 0] leading to straight lines
                    based off of the cubics fit on the inside. *)
    | Clamped of float * float
                (** The 2nd derivatives at the end points of the
                    spline are equal to the passed functions.
                    S'(x_0) = f'(x_0) && S'(x_n) = f'(x_n). *)
 

  type t (*= float * float * float * float * float) array *)

  (** [knots t] return the points used to originally fit the spline. *)
  val knots : t -> (float * float) array 

  (** [fit ~bc data]

    @param bc defaults to [Natural]
    @param data is sorted during the fit.
   *)
  val fit : ?bc:boundary_condition -> (float * float) array -> t

  (*val eval_at : t -> int -> float -> float *)
  (** [eval spline x] evalute [spline] at [x].
   
    Note that if [x] is outside the original points used to fit the spline,
    the fit to the closest knot is used. *)
  val eval : t -> float -> float
  val eval_arr : t -> float array -> float array

end

val lagrange : (float * float) array -> float -> float
