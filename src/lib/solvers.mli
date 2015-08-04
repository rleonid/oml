(** Numerically find the roots of passed functions. *)

(** [newton_raphson_full ?init accuracy iterations lower_bound upper_bound f df]
    uses the Newton Raphson method to iteratively solve [f].

    Starting at mid point of [lower_bound] and [upper_bound], the method
    uses [df] to follow the tangent to it's root, that becomes the new point.
    Iteration continues until [f x] < [accuracy].

    @raise Util.IterationFailure if we escape our bounds or take longer than
      [iterations].
*)
val newton_raphson_full :
  ?init:float -> accuracy:float -> iterations:int -> lower_bound:float ->
    upper_bound:float -> (float -> float) -> (float -> float) -> float

(** [newton ?init lower_bound upper_bound f] is equivalent to
    [newton_raphson_full ?init ~accuracy:1.0e-10 ~iterations:1000
      lower_bound upper_bound f (Estimations.second_order f)] *)
val newton : ?init:float -> lower_bound:float ->
            upper_bound:float -> (float -> float) -> float

(* Commented out because of bug
(** [bisection epsilon lower_bound upper_bound f] iteratively
    finds the root of [f] between [lower_bound] and [upper_bound],
    or the closer bound if there is not root therein. *)
val bisection : epsilon:float -> lower_bound:float -> upper_bound:float ->
    (float -> float) -> float
    *)
