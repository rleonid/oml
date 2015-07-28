
(** When we do not know the mean or standard deviation of a distribution
    we can still create a prediction interval based off distribution stat.
*)
val prediction_interval : float -> Descriptive.summary -> float * float

(** A hypothesis test. *)
type test =
  { standard_error     : float
  ; degrees_of_freedom : float
  ; stat               : float
  ; prob_by_chance     : float (** the probability that |t_stat| could be this
                                   large (or larger) by chance, for
                                   distributions with equal means. *)
  }

(** Describe a hypothesis test. *)
val test_to_string : test -> string

(** [chi observed expected] computes Pearson's Chi squared test of drawing
  [observed] data from the the same categorical distribution as [expected]. *)
val chi : float array -> float array -> test

type null_hypothesis =
  | TwoTail   (* the sample mean equals the population mean. *)
  | OneTail   (* the sample mean is less than or greater than
                 the population mean. *)

val simple_t_test : null_hypothesis -> int -> float -> float -> test

val mean_t_test : float -> null_hypothesis -> float array -> test

val equal_means_same_variance_test : null_hypothesis -> float array
                                      -> float array -> test

val unequal_variance_test : null_hypothesis -> float array -> float array -> test

val different_variances_test : float array -> float array -> test

val correlation_test : float array -> float array -> test
