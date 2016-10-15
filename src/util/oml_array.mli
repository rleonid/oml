(*
   Copyright 2015,2016:
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

(** Extend the [Array] module with useful functions. *)

include module type of Array

(** [Array.fold2 f x a b] computes
      [f (... (f (f x a.(0) b.(0)) a.(1) b.(1)) ...) a.(n-1) b.(n-1)].

      @raise Invalid_argument if the lengths are unequal. *)
val fold2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b array -> 'c array -> 'a

(** [Array.map2 f a b] applies [f] to all of the aligned elements of [a] and
    [b].

    @raise Invalid_argument if the lengths are unequal. *)
val map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array

(** [Array.sumf a] computes the sum of the elements of [a] using
    {{:https://en.wikipedia.org/wiki/Kahan_summation_algorithm}Kahan summation algorithm}. *)
val sumf : float array -> float

(** [Array.prodf a] computes [a.(0) *. a.(1) *. ... a.(n-1)], naively. *)
val prodf : float array -> float

(** [Array.max a] computes [max a.(0) (max a.(1)  ... (max a.(n-2) a.(n-1)))].
  *)
val max : 'a array -> 'a

(** [Array.min a] computes [min a.(0) (min a.(1)  ... (min a.(n-2) a.(n-1)))].
  *)
val min : 'a array -> 'a

(** [Array.find_index f a] returns the first [i] (starting with 0) where
    [f a.(i)] is true.

    @param start The index from which we start searching, defaults to 0. *)
val find_index : ?start:int -> ('a -> bool) -> 'a array -> int

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

(** [zip x y] construct an array that stores the elements of x and y as a
    tuple.

    @raise Invalid_argument if the arrays of unequal length. *)
val zip : 'a array -> 'b array -> ('a * 'b) array

(** [unzip arr] split an array of tuples into an array of the first value and
    an array of the second. *)
val unzip : ('a * 'b) array -> ('a array * 'b array)

(** [permute arr] will permute a copy of [arr].

    @param copy can be set to false to perform the permutation in place. *)
val permute : ?copy:bool -> 'a array -> 'a array
