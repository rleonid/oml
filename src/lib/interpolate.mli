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
    at the fit data points. *)
module Spline : sig
  type cubic_spline_boundary_condition = NaturalCubic | Clamped
  type t (*= float * float * float * float * float) array *)
  val eval_at : t -> int -> float -> float
  val eval : t -> float -> float
  val eval_arr : t -> float array -> float array
  val fit : ?sorted:bool -> 'a -> (float * float) array -> t
  val lagrange : (float * float) array -> float -> float
end
