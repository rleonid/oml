(** Create generators for sampling from specified distributions. *)

(** [normal ?seed mean std ()] creates a generator that will return variables
    from the Normal distribution of [mean] and [std] (standard deviation).*)
val normal : ?seed:int array -> mean:float -> std:float -> unit -> (unit ->
  float)

(** [normal_std seed ()] is equivalent to [normal seed ~mean:0.0 ~std:1.0 ()].*)
val normal_std : ?seed:int array -> unit -> (unit -> float)

(** [uniform_i ?seed n] creates a generator that will return an integer
    with equal probabilities from the interval [0,n).

    @raise Invalid_argument if [n] is zero or negative. *)
val uniform_i : ?seed:int array -> int -> (unit -> int)

(** [uniform_f ?seed n] creates a generator that will return a float
    with equal probabilities from the interval [0,n].

    @raise Invalid_argument if [n] is zero or negative. *)
val uniform_f : ?seed:int array -> float -> (unit -> float)

(** [multinomial ?seed weights] creates a generator that will return an integer
    representating the ith element from the Multinomial distribution given by
    a [weights] vector which sums to [1].

    @raise Invalid_argument if [weights] do not sum to [1.0] *)
val multinomial : ?seed:int array -> float array -> (unit -> int)

(** [softmax ?seed ?temp weights] creates a generator that will return an integer
    representating the ith element from the softmax distribution given by
    a [weights] vector and [temp]erature parameter which defaults to [1.0].

    @raise Invalid_argument if [weights] is empty *)
val softmax : ?seed:int array -> ?temp:float -> float array -> (unit -> int)

(** Provides polymorphic versions that sample over arrays of any type *)
module Poly :
  sig
    (** [uniform ?seed elems] creates a generator that will sample from the
        [elems] array using the uniformly random distribution.

        @raise Invalid_argument if the given element array is empty. *)
    val uniform : ?seed:int array -> 'a array -> (unit -> 'a)

    (** [multinomial ?seed elems weights] creates a generator will sample from
        the [elems] array using Multinomial distribution given by
        a [weights] vector which sums to [1].

        @raise Invalid_argument if [weights] do not sum to [1.0] or
        the length of the [elems] and [weights] arrays are not equal. *)
    val multinomial : ?seed:int array -> 'a array -> float array -> (unit -> 'a)

    (** [softmax ?seed ?temp elems weights] creates a generator that will
        sample from the [elems] array using the softmax distribution given by
        a [weights] vector and [temp]erature parameter which defaults to [1.0].

        @raise Invalid_argument if [weights] is empty or the length of the
        [elems] and [weights] arrays are not equal. *)
    val softmax : ?seed:int array -> ?temp:float ->'a array ->
      float array -> (unit -> 'a)
  end
