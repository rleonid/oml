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

(** Provide an interface to treat float array's as vectors. *)

type t = float array

(** [equal d x y] two vectors are equal if they have the same length and all
  pairwise elements are not [Util.significantly_different_from ?d] . *)
val equal : ?d:float -> t -> t -> bool

(** [add x y] add pairwise elements. *)
val add : t -> t -> t

(** [sub x y] subtract pairwise elements. *)
val sub : t -> t -> t

(** [mult s v] multiplies each element of [v] by scalar [s]. *)
val mult : float -> t -> t

(** [dot x y] computes dot product. *)
val dot : t -> t -> float
