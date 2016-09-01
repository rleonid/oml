(** OCaml Math, Statistics and ML Libary (Pure OCaml). *)

(** Common values used throughout the library. *)
module Util : sig
  include module type of Util
end

(** WIP: Special functions and Linear Algebra. *)
module Uncategorized : sig
  include module type of Oml_lite_uncategorized
end

(** Inference, parameter estimation.*)
module Statistics : sig
  module Continued_fraction : sig include module type of Continued_fraction end
  module Functions : sig include module type of Oml_lite_functions end
  module Sampling : sig include module type of Sampling end
  module Descriptive : sig include module type of Descriptive end
  module Measures : sig include module type of Measures end
end

(** Compute running statitics using recurrence equations. *)
module Online : sig include module type of Online end

(** Classify data based on features. *)
module Classification : sig
  module Intf : sig include module type of Cls_intf end
  module Probabilities : sig include module type of Probabilities end
  module Naive_bayes : sig
    include module type of Oml_lite_naive_bayes
  end
  module Performance : sig include module type of Performance end
end

(** Model relationship between variables. *)
module Regression : sig
  module Intf : sig include module type of Oml_lite_intf end
  module Univariate : sig include module type of Oml_lite_univariate end
  module Interpolate : sig include module type of Interpolate end
end
