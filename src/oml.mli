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
  module Matrices : sig

    type t = Vectors.t array

    (** [row m i] returns the [i]th row of matrix [m].*)
    val row : t -> int -> Vectors.t

    (** [column m i] returns the [i]th colum of matrix [m].*)
    val column : t -> int -> Vectors.t

    (** [dim m] the dimensions (row, columns) of the matrix [m]. *)
    val dim : t -> int * int

    (** [transpose m] returns the transpose of [m]. *)
    val transpose : t -> t

    (** [diagonal v] create a diagonal matrix from vector [v]. *)
    val diagonal : ?n:int -> ?m:int -> Vectors.t -> t

    (** [equal d x y] two matrices are equal if they have the same dimensions
      and all pairwise elements are not [Util.significantly_different_from ?d]
      from each other. *)
    val equal : ?d:float -> t -> t -> bool

    (** [add x y] add two matrices. *)
    val add : t -> t -> t

    (** [sub x y] subtraction. *)
    val sub : t -> t -> t

    (** [mult s v] scalar multiplication. *)
    val mult : float -> t -> t

    (** [identity n] create the identity matrix of rank [n]. *)
    val identity : int -> t

    (** [prod m n] matrix product [m * n]

        @raise Invalid_argument if matrix sizes are not compatible. *)
    val prod : t -> t -> t

    (** Multiply a row vector against a matrix. *)
    val prod_row_vector : Vectors.t -> t -> Vectors.t

    (** Multiply a matrix against a column vector. *)
    val prod_column_vector : t -> Vectors.t -> Vectors.t
  end
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
  module Probabilities : sig include module type of Oml_probabilities end
  module Interfaces : sig include module type of Oml_classification_interfaces end
  module Naive_bayes : sig
    (** Train a
      {{:https://en.wikipedia.org/wiki/Naive_Bayes_classifier}Naive Bayes}
      classifier on data encoded using
      {{!modtype:Cls_intf.Dummy_encoded_data}Dummy variables.} *)
    module Binomial(D: Interfaces.Dummy_encoded_data) : sig
      include Interfaces.Generative
          with type feature = D.feature
          and type class_ = D.class_
          and type feature_probability = float array

      (** [opt ~smoothing ~bernoulli ()] the optional configuration of the
          classifier.

          @param bernouli if true we treat the underlying distribution as Bernoulli
                          (as opposed to Multinomial) and estimate the likelihood
                          with (1-p_i) for features [i] that are missing from a
                          feature when {{!val:eval}evaluated}.
          @param smoothing
            {{:http://en.wikipedia.org/wiki/Additive_smoothing}Additive smoothing}
            can be applied to the final estimate of Naive Bayes classifiers.
            When estimating a probability distribution by counting observed instances
            in the feature space we may want to smooth the values, particularly if our
            training data is sparse. *)
      val opt : ?smoothing:float -> ?bernoulli:bool -> unit -> opt

    end

    (** Train a
      {{:https://en.wikipedia.org/wiki/Naive_Bayes_classifier}Naive Bayes}
      classifier on data encoded using
      {{!modtype:Cls_intf.Category_encoded_data}Categorical variables.} *)
    module Categorical(D: Interfaces.Category_encoded_data) : sig
      include Interfaces.Generative
          with type feature = D.feature
          and type class_ = D.class_
          and type feature_probability = float array

      (** [opt ~smoothing ()] the optional configuration of the classifier.

          @param smoothing
            {{:http://en.wikipedia.org/wiki/Additive_smoothing}Additive smoothing}
            can be applied to the final estimate of Naive Bayes classifiers.
            When estimating a probability distribution by counting observed instances
            in the feature space we may want to smooth the values, particularly if our
            training data is sparse. *)
      val opt : ?smoothing:float -> unit -> opt

    end
  end
  module Performance : sig include module type of Oml_performance end
end

(** Model relationship between variables. *)
module Regression : sig
  module Interfaces : sig include module type of Oml_regression_interfaces end
  module Univariate : sig include module type of Oml_univariate end
  module Interpolate : sig include module type of Oml_interpolate end
end
