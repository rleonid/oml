
(** Simple one dimensional regression. *)

(** The optional [opt] for univariate regression are weights for each
    observation. One can use them to change the model such that each
    error (e_i) is now sampled from it's own distribution: [N(0, s/w_i)],
    where s^2 is the error variance and w_i is the weight of the ith
    error. *)
type opt = float array

val opt : ?weights:float array -> unit -> opt

include Intf.Linear_model
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
val alpha_test : ?null:float -> t -> Statistics.Hypothesis_test.t

(** [beta_test ~null linear_model] perform a hypothesis test on the [beta]
    coefficient of the [linear_model]. *)
val beta_test : ?null:float -> t -> Statistics.Hypothesis_test.t
