(*
   Copyright 2016:
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

(** A more summation that keeps track of low order bits. *)
type t

(** [empty] initialize a summation. *)
val empty : t

val zero : t

(** [update summation value] *)
val update : t -> float -> t

(** [update_with_degenerate_check summation value]
    Does not update the sum if it is currently at a degenerate
    (infinity, neg_infinity or nan) value. *)
val update_with_degenerate_check : t -> float -> t

(** [update summation value] *)
val ( + ) : t -> float -> t

(** [sum summation] returns the sum from summation. *)
val sum : t -> float
