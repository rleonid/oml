module Util = Util

module Uncategorized = Oml_lite_uncategorized

module Statistics = struct
  module Continued_fraction = Continued_fraction
  module Functions = Oml_lite_functions
  module Sampling = Sampling
  module Descriptive = Descriptive
  module Measures = Measures
end

module Online = Online

module Classification = struct
  module Intf = Cls_intf
  module Probabilities = Probabilities
  module Naive_bayes = Oml_lite_naive_bayes
  module Performance = Performance
end

module Regression = struct
  module Intf = Oml_lite_intf
  module Univariate = Oml_lite_univariate
  module Interpolate = Interpolate
end
