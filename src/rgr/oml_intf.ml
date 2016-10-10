(*
   Copyright 2015,2016:
     Leonid Rozenberg <leonidr@gmail.com>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

(** The interface of the model constructed by a Regression procedure. *)
module type Linear_model = sig
  include Oml_util.Optional_arg_intf

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

  (** [F_test linear_model] compute the F-statistic to assess if there is any
      relationship between the response and predictors in the [linear_model].*)
  val f_statistic : t -> float (*Hypothesis_test.t*)

end
