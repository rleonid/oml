module Util = struct include Util end

module Uncategorized = struct
  include Oml_uncategorized
end

module Statistics = struct
  module Continued_fraction = Continued_fraction
  module Functions = struct
    include Oml_functions
  end

  module Sampling = Sampling
  module Descriptive = Descriptive
  module Measures = Measures
  module Distributions = Distributions
  module Hypothesis_test = Hypothesis_test
end

module Online = Online

module Classification = struct
  module Intf = Cls_intf
  module Probabilities = Probabilities
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
  module Interpolate = Interpolate

end
