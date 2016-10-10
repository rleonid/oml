(** OCaml Math, Statistics and ML Libary (Pure OCaml). *)

(** Common values used throughout the library. *)
module Util : sig
  include module type of Oml_util
end

(** WIP: Special functions and Linear Algebra. *)
module Uncategorized : sig
  include module type of Oml_uncategorized
end

(** Inference, parameter estimation.*)
module Statistics : sig
  module Continued_fraction : sig include
    module type of Oml_continued_fraction
  end
  module Functions : sig include module type of Oml_functions end
  module Sampling : sig include module type of Oml_sampling end
  module Descriptive : sig include module type of Oml_descriptive end
  module Measures : sig include module type of Oml_measures end
end

(** Compute running statitics using recurrence equations. *)
module Online : sig include module type of Oml_online end

(** Classify data based on features. *)
module Classification : sig
  module Intf : sig include module type of Oml_cls_intf end
  module Probabilities : sig include module type of Oml_probabilities end
  module Naive_bayes : sig
    include module type of Oml_naive_bayes
  end
  module Performance : sig include module type of Oml_performance end
end

(** Model relationship between variables. *)
module Regression : sig
  module Intf : sig include module type of Oml_intf end
  module Univariate : sig include module type of Oml_univariate end
  module Interpolate : sig include module type of Oml_interpolate end
end
