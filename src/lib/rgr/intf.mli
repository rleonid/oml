
(** The interface of the model constructed by a Regression procedure. *)
module type Linear_model = sig
  include Util.Optional_arg_intf

  type input
  type t

  (** [describe t] returns a string describing the regressed linear model.*)
  val describe : t -> string

  (** [eval linear_model x] Evaluate a the [linear_model] at [x].*)
  val eval : t -> input -> float

  (** [regress options pred resp] computes a linear model of [resp] based
      off of the independent variables in the design matrix [pred], taking
      into account the various method [opt]s. *)
  val regress : ?opt:opt -> input array -> resp:float array -> t

  (** [residuals t] returns the residuals, the difference between the observed
      value and the estimated value for the independent, response, values. *)
  val residuals : t -> float array

  (** [coefficients t] returns the coefficients used in the linear model. *)
  val coefficients : t -> float array

  (** [residual_standard_error linear_model] returns an estimate, based on the
      residuals, of the variance of the error term in the linear model.*)
  val residual_standard_error : t -> float

  (** [coeff_of_determination linear_model] returns the R^2 statistic for the
      linear model. *)
  val coeff_of_determination : t -> float

  (** [confidence_interval linear_model alpha x] Use the [linear_model] to
      construct confidence intervals at [x] at an [alpha]-level of significance.
  *)
  val confidence_interval : t -> alpha:float -> input -> float * float

  (** [prediction_interval linear_model alpha x] Use the [linear_model] to
      construct prediction intervals at [x] at an [alpha]-level of significance.
  *)
  val prediction_interval : t -> alpha:float -> input -> float * float

  (** [coefficient_tests linear_model] perform hypothesis tests on the
      models coefficients to see if they are significantly different from
      the null. *)
  val coefficient_tests : ?null:float -> t -> Statistics.Hypothesis_test.t array

  (** [F_test linear_model] compute the F-statistic to assess if there is any
      relationship between the response and predictors in the [linear_model].*)
  val f_statistic : t -> float (*Hypothesis_test.t*)

end


