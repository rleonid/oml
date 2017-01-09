module Util = Oml.Util

module Uncategorized = struct
  include Oml.Uncategorized
  module Lacaml_util = Omlf_lacaml_util
  module Svd = Omlf_svd
end

module Statistics = struct
  module Continued_fraction = Oml.Statistics.Continued_fraction
  module Functions = Omlf_functions
  module Sampling = Oml.Statistics.Sampling
  module Descriptive = Oml.Statistics.Descriptive
  module Measures = Oml.Statistics.Measures
  module Distributions = Omlf_distributions
  module Hypothesis_test = Omlf_hypothesis_test
end

module Online = Oml.Online

module Classification = struct
  module Interfaces = Oml.Classification.Interfaces
  module Probabilities = Oml.Classification.Probabilities
  module Naive_bayes = Omlf_naive_bayes
  module Logistic_regression = Omlf_logistic_regression
  module Descriminant = Omlf_descriminant
  module Performance = Oml.Classification.Performance
end

module Regression = struct
  module Interfaces = struct
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

  module Univariate = Omlf_univariate
  module Multivariate = Omlf_multivariate
  module Tikhonov = Omlf_tikhonov
  module Interpolate = Oml.Regression.Interpolate
end

module Unsupervised = struct
  module Pca = Omlf_pca
end
