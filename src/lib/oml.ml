module Util = struct include Oml_lite.Util end

module Uncategorized = struct
  include Oml_uncategorized
end

module Statistics = struct
  module Continued_fraction = Oml_lite.Statistics.Continued_fraction
  module Functions = struct
    include Oml_functions
  end
  module Sampling = Oml_lite.Statistics.Sampling
  module Descriptive = Oml_lite.Statistics.Descriptive
  module Measures = Oml_lite.Statistics.Measures
  module Distributions = Distributions
  module Hypothesis_test = Hypothesis_test
end

module Online = Online

module Classification = struct
  module Intf = Oml_lite.Classification.Intf
  module Probabilities = Oml_lite.Classification.Probabilities
  module Naive_bayes = struct
    include Oml_naive_bayes
  end
  module Logistic_regression = Logistic_regression
  module Descriminant = Descriminant
  module Performance = Performance
end

module Regression = struct
  module Intf = struct
    include Oml_intf
  end

  module Univariate = struct
    include Oml_univariate
  end
  module Multivariate = Multivariate
  module Tikhonov = Tikhonov
  module Interpolate = Oml_lite.Regression.Interpolate
end

module Unsupervised = struct
  module Pca = Pca
end
