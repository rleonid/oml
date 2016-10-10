(*
   Copyright 2015:2016
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

type opt = float array

val opt : ?weights:float array -> unit -> opt

include Omlf_intf.Linear_model
  with type input = float
  and type opt := opt

(** [alpha t] a shorthand for the constant parameter used in the regression.
    Equivalent to [(coefficients t).(0)] *)
val alpha : t -> float

(** [beta t] a shorthand for the linear parameter used in the regression.
    Equivalent to [(coefficients t).(1)] *)
val beta : t -> float

(** [alpha_test ~null linear_model] perform a hypothesis test on the [alpha]
    coefficient of the [linear_model]. *)
val alpha_test : ?null:float -> t -> Omlf_hypothesis_test.t

(** [beta_test ~null linear_model] perform a hypothesis test on the [beta]
    coefficient of the [linear_model]. *)
val beta_test : ?null:float -> t -> Omlf_hypothesis_test.t
