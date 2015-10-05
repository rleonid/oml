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

(** {2 Utilities}

+ general methods that are reused in other modules.
+ overrides and 
+ extensions of standard library modules
    (ex. {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Array.html}Array})
    with useful methods (ex. {!Array.fold2} and {!Array.sumf})
*)

(** Some of the iterative routines can fail for the following reasons. *)
type iterative_failure_reason =
  | Out_of_bounds of float
  | No_convergence
  | Too_many_iterations of int
  | Too_few_iterations of int

(** The exception raised when an iterative function fails to converge. *)
exception Iteration_failure of string * iterative_failure_reason

val invalidArg : ('a, unit, string, 'b) format4 -> 'a

(** 3.14159265358979312  *)
val pi : float

val midpoint : float -> float -> float

(** Extend the [Array] module with useful functions. *) 
module Array : sig
  include (module type of Array)

  (** [Array.fold2 f x a b] computes
       [f (... (f (f x a.(0) b.(0)) a.(1) b.(1)) ...) a.(n-1) b.(n-1)].

       @raise Invalid_argument if the lengths are unequal. *)
  val fold2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b array -> 'c array -> 'a

  (** [Array.map2 f a b] applies [f] to all of the aligned elements of [a] and
      [b].
      
      @raise Invalid_argument if the lengths are unequal. *)
  val map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array

  (** [Array.sumf a] computes [a.(0) +. a.(1) +. ... a.(n-1)]. *)
  val sumf : float array -> float

  (** [Array.prodf a] computes [a.(0) *. a.(1) *. ... a.(n-1)]. *)
  val prodf : float array -> float

  (** [Array.max a] computes [max a.(0) (max a.(1)  ... (max a.(n-2) a.(n-1)))].
   *)
  val max : 'a array -> 'a

  (** [Array.min a] computes [min a.(0) (min a.(1)  ... (min a.(n-2) a.(n-1)))].
   *)
  val min : 'a array -> 'a

  (** [Array.find_index f a] returns the first [i] (starting with 0) where
      [f a.(i)] is true *)
  val find_index : ('a -> bool) -> 'a array -> int

  (** [binary_search c a], find the index of element [e] in [a] where
      [c e = 0]. Where [c e'] returns < 0 if [e' < e] and [> 0] if [e' > e].

      If no element is found where [c e = 0] then the largest index such that
      [c e < -1] is returned, this might be (-1). *)
  val binary_search : ('a -> int) -> 'a array -> int

  (** [Array.binary_search_exn c a], find the index of an element [e] in [a] where
      [c e = 0].  Where [c e'] returns < 0 if [e' < e] and [> 0] if [e' > e].

      @raise Not_found if no element is found where [c e = 0]. *)
  val binary_search_exn : ('a -> int) -> 'a array -> int

  (** [all p arr] equivalent to [true && (p arr.(0)) && (p arr.(1)) ...
      && (p arr.(n))] *)
  val all : ('a -> bool) -> 'a array -> bool

  (** [any p arr] equivalent to [false || (p arr.(0)) || (p arr.(1)) ...
      || (p arr.(n))] *)
  val any : ('a -> bool) -> 'a array -> bool

  (** [has_order c arr] equivalent to
      [ c arr.(0) arr.(1) && c arr.(1) arr.(2) ... c arr.(n-1) arr.(n)]
      but short circuits. *)
  val has_order : ('a -> 'a -> bool) -> 'a array -> bool

  (** [range incr start stop] creates a float array of all the values in the
      interval [\[start,stop)], counting by [incr] (defaults to [1.0]). *)
  val range : ?incr:float -> start:float -> stop:float -> unit -> float array

  (** [ranks ?start ?average_ties ?compare arr] returns an array where each
     element [arr.(i)] is replaced by it's rank in [arr]. The first element is
     [start] (defaults to 1).

     Optionally [average_ties] can be specified if the assigned ranks should
     be averaged together (ex. [|1;2;3|] -> [|2;2;2|]).
     [compare] can also be specified if more granularity is desired over float
     comparison. *)
  val ranks : ?start:int -> ?average_ties:bool -> ?compare:(float -> float -> int)
              -> float array -> float array

end

(** A really small value. Also known as the machine epsilon, the smallest
    distance between two representable floats. *)
val dx : float

(** [significantly_different_from ?d x y] will check if [y] is more than [d]
    (defaults to [dx]) away from [x]. *)
val significantly_different_from : ?d:float -> float -> float -> bool

(** [equal_floats d x y] compares [x] and [y] too see if they are equal,
    specifically [not (significantly_different_from ~d x y)].*)
val equal_floats : d:float -> float -> float -> bool

(** [is_nan x] robustly determine if a float represents NaN (aka Not a number). *)
val is_nan : float -> bool

(** [is_degenerate x] determine if a value is [nan] or [neg_infinity] or
    [infinity]. *)
val is_degenerate : float -> bool

type 'a bound = Open of 'a
              | Closed of 'a

(** [within bound_pair x] evaluates whether [x] is inside the interval described
    by the [bound_pair]. *)
val within : ('a bound * 'a bound) -> 'a -> bool

(** Provide operator overloads for conveniently expressing float arithmatic.*)
module Float : sig
  val ( + ) : float -> float -> float
  val ( - ) : float -> float -> float
  val ( / ) : float -> float -> float
  val ( * ) : float -> float -> float

end

(** When passing optional arguments to procedures. *)
module type Optional_arg_intf = sig

  type spec             (** type of default argument. *)
  val default : spec    (** A default value used when not specified.*)
end


val fst3 : ('a * 'b * 'c) -> 'a
val snd3 : ('a * 'b * 'c) -> 'b
val thr3 : ('a * 'b * 'c) -> 'c

