
(** Some of the iterative routines can fail for the following reasons. *)
type iterative_failure_reason =
  | OutOfBounds of float
  | NoConvergence
  | TooManyIterations of int
  | TooFewIterations of int

(** The exception raised when an iterative function fails to converge. *)
exception IterationFailure of string * iterative_failure_reason

val invalidArg : ('a, unit, string, 'b) format4 -> 'a

(** 3.14159265358979312  *)
val pi : float

val midpoint : float -> float -> float

module Array : sig
  include (module type of Array)

  (** [Array.fold2 f x a b] computes \
       [f (... (f (f x a.(0) b.(0)) a.(1) b.(1)) ...) a.(n-1) b.(n-1)].
       Raises [Invalid_argument] if the lengths are unequal.  *)
  val fold2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b array -> 'c array -> 'a

  (** [Array.map2 f a b] applies [f] to all of the aligned elements of [a] and
      [b], Raises [Invalid_argument] if the lengths are unequal.  *)
  val map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array

  (** [Array.sumf a] computes [a.(0) +. a.(1) +. ... a.(n-1)]. *)
  val sumf : float array -> float

  (** [Array.prodf a] computes [a.(0) *. a.(1) *. ... a.(n-1)]. *)
  val prodf : float array -> float

  (** [Array.max a] computes [max a.(0) (max a.(1)  ... (max a.(n-2) a.(n-1))].
   *)
  val max : 'a array -> 'a

  (** [Array.min a] computes [min a.(0) (min a.(1)  ... (min a.(n-2) a.(n-1))].
   *)
  val min : 'a array -> 'a

  (** [Array.find_index f a] returns the first [i] (starting with 0) where
      [f a.(i)] is true *)
  val find_index : ('a -> bool) -> 'a array -> int

  (** [Array.binary_search c a], finds an element [e] in [a] where [c e = 0].
      Where [c e'] returns < 0 if [e' < e] and [> 0] if [e' > e]. *)
  val binary_search : ('a -> int) -> 'a array -> 'a

  (** [all p arr] equivalent to [true && (p arr.(0)) && (p arr.(1)) ...
      && (p arr.(n))] *)
  val all : ('a -> bool) -> 'a array -> bool

  (** [any p arr] equivalent to [false || (p arr.(0)) || (p arr.(1)) ...
      || (p arr.(n))] *)
  val any : ('a -> bool) -> 'a array -> bool

  (** [range incr start stop] create a float elements of all values in the
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

(** [is_nan x] robustly determine if a float represents NaN (aka Not a number). *)
val is_nan : float -> bool

(** [is_degenerate x] determine if a value is [nan] or [neg_infinity] or
    [infinity]. *)
val is_degenerate : float -> bool
