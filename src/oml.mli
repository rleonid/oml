(** OCaml Math, Statistics and ML Libary (Pure OCaml). *)

(** Common values used throughout the library. *)
module Util : sig
  include module type of Oml_util
end

(** WIP: Special functions and Linear Algebra. *)
module Uncategorized : sig
  module Estimations : sig include module type of Oml_estimations end
  module Solvers : sig include module type of Oml_solvers end
  module Vectors : sig include module type of Oml_vectors end
  module Matrices : sig include module type of Oml_matrices end
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
  module Interfaces : sig include module type of Oml_classification_interfaces end
  module Probabilities : sig include module type of Oml_probabilities end
  module Naive_bayes : sig
    include module type of Oml_naive_bayes
  end
  module Performance : sig include module type of Oml_performance end
end

(** Model relationship between variables. *)
module Regression : sig
  module Interfaces : sig include module type of Oml_regression_interfaces end
  module Univariate : sig include module type of Oml_univariate end
  module Interpolate : sig include module type of Oml_interpolate end
end
