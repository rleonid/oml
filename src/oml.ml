module Util = Oml_util

module Uncategorized = struct
  module Estimations = Oml_estimations
  module Solvers = Oml_solvers
  module Vectors = Oml_vectors
  module Matrices = Oml_matrices
end

module Statistics = struct
  module Continued_fraction = Oml_continued_fraction
  module Functions = Oml_functions
  module Sampling = Oml_sampling
  module Descriptive = Oml_descriptive
  module Measures = Oml_measures
end

module Online = Oml_online

module Classification = struct
  module Probabilities = Oml_probabilities
  module Input_interfaces = Oml_classification_input_interfaces
  module Classifier_interfaces = struct
    module type Classifier = sig
      include Input_interfaces.Data
      include Oml_util.Optional_arg_intf
      type t
      val eval : t -> feature -> class_ Probabilities.t
      type samples = (class_ * feature) list
      val estimate : ?opt:opt -> ?classes:class_ list -> samples -> t
    end

    module type Generative = sig
      include Classifier
      type feature_probability
      val class_probabilities : t -> class_ -> float * (feature -> feature_probability)
    end
  end
  module Naive_bayes = Oml_naive_bayes
  module Performance = Oml_performance
end

module Regression = struct
  module Interfaces = Oml_regression_interfaces
  module Univariate = Oml_univariate
  module Interpolate = Oml_interpolate
end
