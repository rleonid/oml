
module type LINEAR_MODEL = sig

  type input
  type t

  (** [describe t] returns a string describing the regressed linear model.*)
  val describe : t -> string

  type spec

  (** [regress options pred resp ()] computes a linear model of [resp] based
      off of the independent variables in the design matrix [pred], taking
      into account the various method [spec]s. *)
  val regress : spec option -> pred:input array -> resp:float array -> unit
                  -> t

  (** [eval linear_model x] Evaluate a the [linear_model] at [x].*)
  val eval : t -> input -> float

  (** [confidence_interval linear_model alpha x] Use the [linear_model] to
      construct confidence intervals at [x] at an [alpha]-level of significance.
  *)
  val confidence_interval : t -> alpha:float -> input -> float * float

  (** [prediction_interval linear_model alpha x] Use the [linear_model] to
      construct prediction intervals at [x] at an [alpha]-level of significance.
  *)
  val prediction_interval : t -> alpha:float -> input -> float * float

end

module Univarite : LINEAR_MODEL

(** A [general_linear_model] is a linear model over a vector space,
    allowing the user to perform multiple linear regression. *)
type general_linear_model =
  { padded                  : bool
  ; g_m_pred                : float array   (** Means of the predicted variables. *)
  ; g_m_resp                : float         (** Mean of the response variable. *)
  ; deg_of_freedom          : float         (** Degree's of freedom in the regression. *)
  ; coefficients            : float array   (** The coefficients of the determined model. *)
  ; correlations            : float array   (** TODO. Document *)
  ; chi_square              : float
  ; g_inferred_response_var : float
  ; sum_squares             : float
  ; cod                     : float         (** coefficient of determination. r^2 *)
  ; adj_cod                 : float         (** adjusted coefficient of determination. r^2 *)
  ; covariance              : float array array (* Covariance matrix. *)
  ; residuals               : float array
  ; aic                     : float
  ; loocv                   : float array   (* Leave-One-Out-Cross-Validation, Predicted Residuals. *)
  }

(** [eval_glm glm data] evaluate the general linear model [glm] over the vector
    of [data]. *)
val eval_glm : general_linear_model -> float array -> float

type lambda_spec =
  [ `Spec of float
  | `From of float array
  | `Within of float * float * float
  ]

(** [general_linear_regress ?lambda ?pad resp pred unit]
  Compute a [general_linear_model] for predicting [resp] based on the design
  matrix [pred].

  [pad] instructs the method to efficiently insert a colum of 1's into the
    design matrix for the constant term.
  [lambda] specifies optional ridge regression logic.
*)
val general_linear_regress : ?lambda:lambda_spec
                            -> ?pad:bool
                            -> resp: float array
                            -> pred: float array array -> unit
                            -> general_linear_model

(** [general_tikhonov_regression ?lambda resp pred tik unit]
  Compute a [general_linear_model] for predicting [resp] based on the design
  matrix [pred] and incorporating [tik] as a regularizer.

  [tik] is the [T^t*T] of the Tikhonov matrix description as
  described here https://en.wikipedia.org/wiki/Tikhonov_regularization.

  [lambda] optionally specify logic for searching for an optimal multiple
  of the [tik] based on the best Leave-One-Out-Cross-Validation.
*)
val general_tikhonov_regression : ?lambda:lambda_spec
                                -> resp: float array
                                -> pred: float array array
                                -> tik: float array array -> unit
                                -> general_linear_model

