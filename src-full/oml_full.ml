module Util = Oml.Util

module Uncategorized = Oml_uncategorized

module Statistics = struct
  module Continued_fraction = Oml.Statistics.Continued_fraction
  module Functions = Oml_functions
  module Sampling = Oml.Statistics.Sampling
  module Descriptive = Oml.Statistics.Descriptive
  module Measures = Oml.Statistics.Measures
  module Distributions = Distributions
  module Hypothesis_test = Hypothesis_test
end

module Online = Oml.Online

module Classification = struct
  module Intf = Oml.Classification.Intf
  module Probabilities = Oml.Classification.Probabilities
  module Naive_bayes = Oml_naive_bayes
  module Logistic_regression = Logistic_regression
  module Descriminant = Descriminant
  module Performance = Oml.Classification.Performance
end

module Regression = struct
  module Intf = Oml_intf
  module Univariate = Oml_univariate
  module Multivariate = Multivariate
  module Tikhonov = Tikhonov
  module Interpolate = Oml.Regression.Interpolate
end

module Unsupervised = struct
  module Pca = Pca
end
