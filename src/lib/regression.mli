
(** A fit linear model. *)
type linear_model =
  { m_pred                : float         (** Mean of predictor variable. *)
  ; m_resp                : float         (** Mean of response variable. *)
  ; size                  : float         (** Number of observations. *)
  ; alpha                 : float         (** Constant term of model. *)
  ; beta                  : float         (** Linear multiplicative term. *)
  ; correlation           : float         (** Pearson correlation of response
                                              to predictor.*)
  ; chi_square            : float         (** Chi^2 of the residuals. *)
  ; inferred_response_var : float         (** *)
  ; goodness_of_fit       : float option  (** *)
  ; s_xx                  : float         (** Sum of squared difference to
                                              [m_pred] useful by interval. *)
  }

(** [eval_lrm linear_model x] evaluate a the [linear_model] at [x]. *)
val eval_lrm : linear_model -> float -> float

(** [linear_regress ?pred_variance resp pred] create a linear model that
    estimates [resp = alpha + beta * pred].

    [pred_variance] represents the assumed variance in the [pred] elements and
    defaults to 1 for all elements. *)
val linear_regress : ?pred_variance:float array -> resp:float array -> pred:float array -> unit -> linear_model

(** [confidence_interval linear_model alpha_level x], given [linear_model]
    compute the alpha (ex 0.95) confidence interval around [x]. *)
val confidence_interval : linear_model -> alpha_level:float -> float -> float * float

(** [prediction_interval linear_model alpha_level x], given [linear_model]
    compute the alpha (ex 0.95) prediction interval around [x]. *)
val prediction_interval : linear_model -> alpha_level:float -> float -> float * float
