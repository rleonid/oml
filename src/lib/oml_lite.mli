module Util : sig
  include module type of Util
end

module Uncategorized : sig
  include module type of Oml_lite_uncategorized
end

module Statistics : sig
  module Continued_fraction : sig include module type of Continued_fraction end
  module Functions : sig 
    include module type of Oml_lite_functions
  end

  module Sampling : sig include module type of Sampling end
  module Descriptive : sig include module type of Descriptive end
  module Measures : sig include module type of Measures end
end

module Online : sig include module type of Online end

module Classification : sig
  module Intf : sig include module type of Cls_intf end
  module Probabilities : sig include module type of Probabilities end
  module Naive_bayes : sig 
    include module type of Oml_lite_naive_bayes
  end
  module Performance : sig include module type of Performance end
end

