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
