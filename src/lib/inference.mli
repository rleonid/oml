(** Infer probabilities from data and perform hypothesis tests. *)

(** [prediction_interval stats alpha]
  Creates a prediction interval for the distribution described by [stats]
  at an [alpha] level of statistical significance; future observations
  will fall within the bounds with probabiltiy [1.0 - alpha].

  When we do not know the mean or standard deviation of a distribution
  we can still create a prediction interval based off of basic sampled
  statistics and Student's distribution.
  *)
val prediction_interval : Descriptive.summary -> float -> float * float

(** A hypothesis test.  *)
type test =
  { degrees_of_freedom : float  (** Can be non-integer due to corrections. *)
  ; statistic          : float  (** The value that we're testing. *)
  ; standard_error     : float  (** The scaled version of the statistic. *)
  ; prob_by_chance     : float  (** The probability that statistic could be
                                    this large (or larger) by chance, for the
                                    specified conditions of the test. *)
  }

(** Describe a hypothesis test. *)
val test_to_string : test -> string

(** [chi observed expected] computes Pearson's Chi squared test of drawing
  [observed] data from the the same categorical distribution as [expected]. *)
val chi : float array -> float array -> test

type null_hypothesis =
  | TwoSided   (* the sample mean equals the population mean. *)
  | OneSided   (* the sample mean is less than or greater than
                  the population mean. *)

(** [t_test nh k d e] conducts a simple T test, against a [nh] null
    hypothesis, where [d] is the difference between population parameter and
    the observed value, [e] is the standard error of the observed value, and
    [k] is the degrees of freedom in the statistical procedure.
    
    One may think of this as a principled way to test the signal (diff)
    to noise (error) seen in a sample of data. *)
val t_test : null_hypothesis -> int -> diff:float -> error:float -> test

(** [mean_t_test population_mean nh samples] conduct a T-test to see if the
    [sample]'s mean is different from the [population_mean] according to the
    null hypothesis [nh].  *)
val mean_t_test : float -> null_hypothesis -> float array -> test

(** [means_same_variance_test nh sample1 sample2] if we can assume that
    [sample1] and [sample2] have the same variance, test whether they have
    the same mean given the null hypothesis [nh]. *)
val means_same_variance_test : null_hypothesis -> float array
                                      -> float array -> test

(** [means_different_variance_test nh sample1 sample2] when we cannot assume
    that [sample1] and [sample2] have the same variance, test whether they
    do indeed have the same mean given the null hypothesis [nh]. AKA Welch's
    test.  *)
val means_different_variance_test : null_hypothesis -> float array
                                      -> float array -> test

(** [variance_ratio_test sample1 sample2] tests the data in [sample1] and
    [sample2] have the same variance based on F-test.*)
val variance_ratio_test : float array -> float array -> test
