(** OCaml Math, Statistics and ML Libary.

    Oml {i plus} [C] and [Fortran] bindings where appropriate.*)

(** Common values used throughout the library. *)
module Util : sig
  include module type of Oml.Util
    with type iterative_failure_reason = Oml.Util.iterative_failure_reason
     and type 'a bound = 'a Oml.Util.bound
     and type Kahan.t = Oml.Util.Kahan.t
end

(** WIP: Special functions and Linear Algebra. *)
module Uncategorized : sig
  include module type of Oml.Uncategorized
    with type Vectors.t = Oml.Uncategorized.Vectors.t
     and type Matrices.t = Oml.Uncategorized.Matrices.t
  module Lacaml_util : module type of Omlf_lacaml_util
  module Svd : module type of Omlf_svd
end

(** Inference, parameter estimation.*)
module Statistics : sig

  module Continued_fraction : sig
    include module type of Oml.Statistics.Continued_fraction
      with type t = Oml.Statistics.Continued_fraction.t
  end
  module Functions : sig
    include module type of Omlf_functions
  end
  module Sampling : sig
    include module type of Oml.Statistics.Sampling
      (* generator type is concrete. *)
  end
  module Descriptive : sig
    include module type of Oml.Statistics.Descriptive
    (* [skew|kurtosis]_classification and summary types are concrete. *)
  end
  module Measures : sig
    include module type of Oml.Statistics.Measures
  end

  module Distributions : sig include module type of Omlf_distributions end

  module Hypothesis_test : sig
    (** Infer probabilities from data and perform hypothesis tests. *)

    (** [prediction_interval stats alpha]
      Creates a prediction interval for the distribution described by [stats]
      at an [alpha] level of statistical significance; future observations
      will fall within the bounds with probabiltiy [1.0 - alpha].

      When we do not know the mean or standard deviation of a distribution
      we can still create a prediction interval based off of basic sampled
      statistics and Student's distribution.
      *)
    val prediction_interval : Oml.Statistics.Descriptive.summary -> float -> float * float

    (** A hypothesis test.  *)
    type t =
      { degrees_of_freedom : float  (** Can be non-integer due to corrections. *)
      ; statistic          : float  (** The value that we're testing. *)
      ; standard_error     : float  (** The scaled version of the statistic. *)
      ; prob_by_chance     : float  (** The probability that statistic could be
                                        this large (or larger) by chance, for the
                                        specified conditions of the test. *)
      }

    (** Describe a hypothesis test. *)
    val test_to_string : t -> string

    (** [chi observed expected] computes Pearson's Chi squared test of drawing
      [observed] data from the the same categorical distribution as [expected]. *)
    val chi : float array -> float array -> t

    type null_hypothesis =
      | Two_sided   (** the sample mean equals the population mean. *)
      | One_sided   (** the sample mean is less than or greater than
                        the population mean. *)

    (** [t_test nh k d e] conducts a simple T test, against a [nh] null
        hypothesis, where [d] is the difference between population parameter and
        the observed value, [e] is the standard error of the observed value, and
        [k] is the degrees of freedom in the statistical procedure.

        One may think of this as a principled way to test the signal (diff)
        to noise (error) seen in a sample of data. *)
    val t_test : null_hypothesis -> degrees_of_freedom:int -> diff:float
                  -> error:float -> t

    (** [mean_t_test population_mean nh samples] conduct a T-test to see if the
        [sample]'s mean is different from the [population_mean] according to the
        null hypothesis [nh].  *)
    val mean_t_test : float -> null_hypothesis -> float array -> t

    (** [means_same_variance_test nh sample1 sample2] if we can assume that
        [sample1] and [sample2] have the same variance, test whether they have
        the same mean given the null hypothesis [nh]. *)
    val means_same_variance_test : null_hypothesis -> float array
                                          -> float array -> t

    (** [means_different_variance_test nh sample1 sample2] when we cannot assume
        that [sample1] and [sample2] have the same variance, test whether they
        do indeed have the same mean given the null hypothesis [nh]. AKA Welch's
        test.  *)
    val means_different_variance_test : null_hypothesis -> float array
                                          -> float array -> t

    (** [variance_ratio_test sample1 sample2] tests the data in [sample1] and
        [sample2] have the same variance based on F-test.*)
    val variance_ratio_test : float array -> float array -> t
    end
  end

(** Compute running statitics using recurrence equations. *)
module Online : sig
  include module type of Oml.Online
    with type t = Oml.Online.t
end

(** Classify data based on features. *)
module Classification : sig
  module Interfaces : sig
    include module type of Oml.Classification.Interfaces
  end
  module Probabilities : sig
    include module type of Oml.Classification.Probabilities
  end
  module Naive_bayes : sig
    include module type of Omlf_naive_bayes
  end
  module Logistic_regression : sig
    include module type of Omlf_logistic_regression
  end
  module Descriminant : sig
    include module type of Omlf_descriminant
  end
  module Performance : sig
    include module type of Oml.Classification.Performance
      (* binary and performance types are concrete. *)
  end
end

(** Model relationship between variables. *)
module Regression : sig
  module Interfaces : sig
    module type Linear_model = sig
      include Oml.Regression.Interfaces.Linear_model

      (** [confidence_interval linear_model alpha x] Use the [linear_model] to
          construct confidence intervals at [x] at an [alpha]-level of significance.
      *)
      val confidence_interval : t -> alpha:float -> input -> float * float

      (** [prediction_interval linear_model alpha x] Use the [linear_model] to
          construct prediction intervals at [x] at an [alpha]-level of significance.
      *)
      val prediction_interval : t -> alpha:float -> input -> float * float

      (** [coefficient_tests linear_model] perform hypothesis tests on the
          models coefficients to see if they are significantly different from
          the null. *)
      val coefficient_tests : ?null:float -> t -> Statistics.Hypothesis_test.t array
    end
  end
  module Univariate : sig
    type opt = float array

    val opt : ?weights:float array -> unit -> opt

    include Interfaces.Linear_model
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
  end
  module Multivariate : sig
    (** Multi-dimensional input regression, with support for Ridge regression. *)

    type opt =
      { add_constant_column : bool  (** Instructs the method to efficiently insert a
                                        column of 1's into the design matrix for the
                                        constant term. *)
      ; l2_regularizer : [`S of float | `From of float array] option
                                    (** How to optionally determine the ridge parameter. *)
      }

    val opt : ?l2_regularizer:[`S of float | `From of float array] ->
              ?add_constant_column:bool ->
              unit ->
              opt

    include Interfaces.Linear_model
      with type input = float array
      and type opt := opt

    (** [aic linear_model] return the Akaike information criterion for the
        [linear_model].*)
    val aic : t -> float

    (** [press linear_model] return the Predicted REsidual Sum of Squares for the
        [linear_model]. *)
    val press : t -> float
  end
  module Tikhonov : sig
    (** Multi-dimensional input regression with a matrix regularizer.
      described {{:https://en.wikipedia.org/wiki/Tikhonov_regularization} here}.

      Please take care with using this method as not all of the algorithms have
      been verified. A warning is printed to standard-error. *)

    type opt =
      { tik_matrix : float array array   (** The regularizing matrix. *)
      ; l2_regularizer : [`S of float | `From of float array] option  (** How to optionally determine the ridge parameter. *)
      }

    val opt : ?tik_matrix:float array array ->
              ?l2_regularizer:[`S of float | `From of float array] ->
              unit ->
              opt

    include Interfaces.Linear_model
        with type input = float array
        and type opt := opt

    (** [aic linear_model] return the Akaike information criterion for the
        [linear_model].*)
    val aic : t -> float

    (** [press linear_model] return the Predicted REsidual Sum of Squares for the
        [linear_model]. *)
    val press : t -> float
  end
  module Interpolate : sig
    include module type of Oml.Regression.Interpolate
      with type Spline.t = Oml.Regression.Interpolate.Spline.t
       (* Spline.boundary is concrete. *)
  end
end

(** Unsupervised learning. *)
module Unsupervised : sig
  module Pca : sig
    include module type of Omlf_pca
  end
end
