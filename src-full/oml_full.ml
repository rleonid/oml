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
  module Intf = Oml.Classification.Intf
  module Probabilities = Oml.Classification.Probabilities
  module Naive_bayes = Omlf_naive_bayes
  module Logistic_regression = Omlf_logistic_regression
  module Descriminant = Omlf_descriminant
  module Performance = Oml.Classification.Performance
end

module Regression = struct
  module Intf = Omlf_intf
  module Univariate = Omlf_univariate
  module Multivariate = Omlf_multivariate
  module Tikhonov = Omlf_tikhonov
  module Interpolate = Oml.Regression.Interpolate
end

module Unsupervised = struct
  module Pca = Omlf_pca
end
