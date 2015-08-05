(** Construct linear models that describe (and learn from) data.*)

(** The interface of the model constructed by a Regression procedure. *)
module type LINEAR_MODEL = sig

  (* TODO: reorder these declarations in a way that makes more sense for
     documentation. *)

  type input
  type t

  (** [describe t] returns a string describing the regressed linear model.*)
  val describe : t -> string

  type spec

  (** [eval linear_model x] Evaluate a the [linear_model] at [x].*)
  val eval : t -> input -> float

  (** [regress options pred resp] computes a linear model of [resp] based
      off of the independent variables in the design matrix [pred], taking
      into account the various method [spec]s. *)
  val regress : spec option -> pred:input array -> resp:float array -> t

  (** [residuals t] returns the residuals, the difference between the observed
      value and the estimated value for the independent, response, values. *)
  val residuals : t -> float array

  (** [coefficients t] returns the coefficients used in the linear model. *)
  val coefficients : t -> float array

end

(** Simple one dimensional regress. *)
module Univarite : sig

  include LINEAR_MODEL
    with type input = float
    and type spec = float array

  (** [alpha t] a shorthand for the constant parameter used in the regression.
      Equivalent to [(coefficients t).(0)] *)
  val alpha : t -> float

  (** [beta t] a shorthand for the linear parameter used in the regression.
      Equivalent to [(coefficients t).(1)] *)
  val beta : t -> float

  (** [confidence_interval linear_model alpha x] Use the [linear_model] to
      construct confidence intervals at [x] at an [alpha]-level of significance.
  *)
  val confidence_interval : t -> alpha:float -> input -> float * float

  (** [prediction_interval linear_model alpha x] Use the [linear_model] to
      construct prediction intervals at [x] at an [alpha]-level of significance.
  *)
  val prediction_interval : t -> alpha:float -> input -> float * float

end

type lambda_spec =
  | Spec of float         (** Use this specific value. *)
  | From of float array   (** Choose the value in the array with the lowest Leave-One-Out-Error. *)

type multivariate_spec =
  { add_constant_column : bool          (** Instructs the method to efficiently insert a colum of 1's into the
                                            design matrix for the constant term. *)
  ; lambda_spec : lambda_spec option    (** How to optionally determine the ridge parameter. *)
  }

(** Multi-dimensional input regression, with support for Ridge regression. *)
module Multivariate : sig

  include LINEAR_MODEL
    with type input = float array
    and type spec = multivariate_spec

end

type tikhonov_spec =
  { regularizer : float array array   (** The regularizing matrix. *)
  ; lambda_spec : lambda_spec option  (** How to optionally determine the ridge parameter. *)
  }

(** Multi-dimensional input regression with a matrix regularizer.
  described {{:https://en.wikipedia.org/wiki/Tikhonov_regularization} here}. *)
module Tikhonov : sig

  include LINEAR_MODEL
    with type input = float array
    and type spec = tikhonov_spec

end
