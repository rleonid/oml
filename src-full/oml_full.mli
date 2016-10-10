(** OCaml Math, Statistics and ML Libary.

    Oml {i plus} [C] and [Fortran] bindings where appropriate.*)

(** Common values used throughout the library. *)
module Util : sig
  include module type of Oml.Util
end

(** WIP: Special functions and Linear Algebra. *)
module Uncategorized : sig
  include module type of Oml_uncategorized
end

(** Inference, parameter estimation.*)
module Statistics : sig

  module Continued_fraction : sig
    include module type of Oml.Statistics.Continued_fraction
  end
  module Functions : sig
    include module type of Oml_functions
  end
  module Sampling : sig
    include module type of Oml.Statistics.Sampling
  end
  module Descriptive : sig
    include module type of Oml.Statistics.Descriptive
  end
  module Measures : sig
    include module type of Oml.Statistics.Measures
  end

  module Distributions : sig include module type of Distributions end
  module Hypothesis_test : sig include module type of Hypothesis_test end
end

(** Compute running statitics using recurrence equations. *)
module Online : sig
  include module type of Oml.Online
        with type t = Oml.Online.t
end

(** Classify data based on features. *)
module Classification : sig
  module Intf : sig
    include module type of Oml.Classification.Intf
  end
  module Probabilities : sig
    include module type of Oml.Classification.Probabilities
  end
  module Naive_bayes : sig
    include module type of Oml_naive_bayes
  end
  module Logistic_regression : sig
    include module type of Logistic_regression
  end
  module Descriminant : sig
    include module type of Descriminant
  end
  module Performance : sig
    include module type of Oml.Classification.Performance
  end
end

(** Model relationship between variables. *)
module Regression : sig
  module Intf : sig
    include module type of Oml_intf
  end

  module Univariate : sig
    include module type of Oml_univariate
  end
  module Multivariate : sig
    include module type of Multivariate
  end
  module Tikhonov : sig
    include module type of Tikhonov
  end
  module Interpolate : sig
    include module type of  Oml.Regression.Interpolate
  end
end

(** Unsupervised learning. *)
module Unsupervised : sig
  module Pca : sig
    include module type of Pca
  end
end
