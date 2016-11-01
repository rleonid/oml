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
  module Interfaces = Oml_classification_interfaces
  module Probabilities = Oml_probabilities
  module Naive_bayes = Oml_naive_bayes
  module Performance = Oml_performance
end

module Regression = struct
  module Interfaces = Oml_regression_interfaces
  module Univariate = Oml_univariate
  module Interpolate = Oml_interpolate
end
