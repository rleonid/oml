module Util = Oml_lite.Util

module Uncategorized = Oml_uncategorized

module Statistics = struct
  module Continued_fraction = Oml_lite.Statistics.Continued_fraction
  module Functions = Oml_functions
  module Sampling = Oml_lite.Statistics.Sampling
  module Descriptive = Oml_lite.Statistics.Descriptive
  module Measures = Oml_lite.Statistics.Measures
  module Distributions = Distributions
  module Hypothesis_test = Hypothesis_test
end

module Online = Oml_lite.Online

module Classification = struct
  module Intf = Oml_lite.Classification.Intf
  module Probabilities = Oml_lite.Classification.Probabilities
  module Naive_bayes = Oml_naive_bayes
  module Logistic_regression = Logistic_regression
  module Descriminant = Descriminant
  module Performance = Performance
end

module Regression = struct
  module Intf = Oml_intf
  module Univariate = Oml_univariate
  module Multivariate = Multivariate
  module Tikhonov = Tikhonov
  module Interpolate = Oml_lite.Regression.Interpolate
end

module Unsupervised = struct
  module Pca = Pca
end
