(*
   Copyright 2015:
     Leonid Rozenberg <leonidr@gmail.com>
     Carmelo Piccione <carmelo.piccione@gmail.com>

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

(** Evaluate continued fraction expressions. *)

type t

(** [init get_a get_b] constructs a continued fraction expression where
    [get_a n x] will return the [n]th constant term at [x] and [get_b n x]
    returns the numerator.*)
val init : get_a:(int -> float -> float) -> get_b:(int -> float -> float) -> t

(** [evaluate t x] evaluates the continued fraction expression at [x] until
    it converges. 
    
    @raise Util.IterationFailure if [t] diverges or it does not converge after
    [200] iterations. *)
val evaluate : t -> ?epsilon:float -> ?max_iterations:int -> float -> float
