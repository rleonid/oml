module Util = struct include Util end

module Uncategorized = struct
  include Oml_lite_uncategorized
end

module Statistics = struct
  module Continued_fraction = Continued_fraction
  module Functions = struct
    include Oml_lite_functions
    include Oml_functions
  end
  module Sampling = Sampling
  module Descriptive = Descriptive
  module Measures = Measures

end

module Online = Online

module Classification = struct
  module Intf = Cls_intf
  module Probabilities = Probabilities
  module Naive_bayes = struct 
    include Oml_lite_naive_bayes
  end
  module Performance = Performance
end

module Regression = struct
  module Intf = struct
    include Oml_lite_intf
  end

  module Univariate = struct
    include Oml_lite_univariate
  end
end
