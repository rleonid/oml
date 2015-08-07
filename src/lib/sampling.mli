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

(** Create generators for sampling from specified distributions. *)

type 'a generator = unit -> 'a

(** [uniform_i ?seed n] creates a generator that will return an integer
    with equal probabilities from the interval \[[0,n]).

    @raise Invalid_argument if [n] is zero or negative, or above [2^30 - 1]. *)
val uniform_i : ?seed:int array -> int -> int generator

(** [uniform_f ?seed n] creates a generator that will return a float
    with equal probabilities from the interval \[[0,n]\].

    @raise Invalid_argument if [n] is zero or negative. *)
val uniform_f : ?seed:int array -> float -> float generator

(** [normal ?seed mean std ()] creates a generator that will return variables
    from the Normal distribution of [mean] and [std] (standard deviation).

    @raise Invalid_argument if [std] is less than zero. *)
val normal : ?seed:int array -> mean:float -> std:float -> unit -> float generator

(** [normal_std seed ()] is equivalent to [normal seed ~mean:0.0 ~std:1.0 ()].*)
val normal_std : ?seed:int array -> unit -> float generator

(** [multinomial ?seed weights] creates a generator that will return an integer
    representating the ith element from the Multinomial distribution given by
    a [weights] vector which sums to [1].

    @raise Invalid_argument if [weights] do not sum to [1.0] (this is checked
     using [Util.significantly_different_from]) or any individual weight is not
     in \[[0,1]). *)
val multinomial : ?seed:int array -> float array -> int generator

(** [softmax ?seed ?temperature weights] creates a generator that will return an integer
    representating the ith element from the softmax distribution given by
    a [weights] vector and [temperature] parameter which defaults to [1.0].

    @raise Invalid_argument if [weights] is empty or [temperature = 0]. *)
val softmax : ?seed:int array -> ?temperature:float -> float array -> int generator

(** Provides polymorphic versions that sample over arrays of any type. *)
module Poly :
  sig
    (** [uniform ?seed elems] creates a generator that will sample from the
        [elems] array using the uniformly random distribution.

        @raise Invalid_argument if the given element array is empty. *)
    val uniform : ?seed:int array -> 'a array -> 'a generator

    (** [multinomial ?seed elems weights] creates a generator will sample from
        the [elems] array using Multinomial distribution given by
        a [weights] vector which sums to [1].

        @raise Invalid_argument if [weights] do not sum to [1.0] or
        the length of the [elems] and [weights] arrays are not equal. *)
    val multinomial : ?seed:int array -> 'a array -> float array -> 'a generator

    (** [softmax ?seed ?temperature elems weights] creates a generator that will
        sample from the [elems] array using the softmax distribution given by
        a [weights] vector and [temperature]erature parameter which defaults to [1.0].

        @raise Invalid_argument if [weights] is empty or the length of the
        [elems] and [weights] arrays are not equal. *)
    val softmax : ?seed:int array -> ?temperature:float ->'a array ->
      float array -> 'a generator
  end
