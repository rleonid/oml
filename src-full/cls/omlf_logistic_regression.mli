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

(** Use
  {{:https://en.wikipedia.org/wiki/Logistic_regression}Logistic Regression}
  to estimate log-odds for each of the quantitative features in the
  {{!modtype:Oml.Classification.Input_interfaces.Continuous_encoded_data}encoded data},
  per class.

  Unless specified via the [classes] argument to [estimate], the first
  class encountered will be considered the {{!base_class}base_class}. All
  other classes will be considered as not [base_class] and [eval] will reflect
  that, by returning the same probability. For multi-class logistic regression
  see {{!module:Multiclass}Multiclass}.

  A constant [1] is added to all encoded features by [estimate],
  there is no need to add one with
  {{!val:Oml.Classification.Input_interfaces.Continuous_encoded_data.encoding}encoding}. *)
module Binary(D: Oml.Classification.Input_interfaces.Continuous_encoded_data) :
  sig
    include Oml.Classification.Classifier_interfaces.Classifier
        with type feature = D.feature
         and type class_ = D.class_

    (** [opt ~lambda ~tolerance ()] a constructor for the optional arguments,

        @param lambda The regularization parameter of the fitting cost.
                           The default value is [0.0001].
        @param tolerance The accuracy tolerance of the underlying L-BFGS
          convergence algorithm. Convergence stops once successive evaluations
          of cost function (scaled) are less than [tolerance * epsilon_float].
          [1e4] is used by default.  See the {Lbfgs.F.min} method for details. *)
    val opt : ?lambda:float -> ?tolerance:float -> unit -> opt

    (** [coefficients t] the weights assigned to the log-odds of the features.

        The first coefficient is for a constant term and therefore
        there will always be 1 more than the [size] of features.
    *)
    val coefficients : t -> float array

    (** [base_class t] returns the class C against which the log-odds
        are computed (and hence coefficients). *)
    val base_class : t -> class_
  end

(** Use {{:https://en.wikipedia.org/wiki/Multinomial_logistic_regression}
  Multiple Class Logistic Regression} to estimate log-odds
  for each of the quantitative features in the
  {{!modtype:Oml.Classification.Input_interfaces.Continuous_encoded_data}encoded data},
  per class: A constant [1] is added to all encoded features by [estimate],
  there is no need to add one with
  {{!val:Oml.Classification.Input_interfaces.Continuous_encoded_data.encoding}encoding}. *)
module Multiclass(D: Oml.Classification.Input_interfaces.Continuous_encoded_data) :
  sig
    include Oml.Classification.Classifier_interfaces.Classifier
        with type feature = D.feature
         and type class_ = D.class_

    (** [opt ~lambda ~tolerance ()] a constructor for the optional arguments,

        @param lambda The regularization parameter of the fitting cost.
                           The default value is [0.0001].
        @param tolerance The accuracy tolerance of the underlying L-BFGS
          convergence algorithm. Convergence stops once successive evaluations
          of cost function (scaled) are less than [tolerance * epsilon_float].
          [1e4] is used by default.  See the {Lbfgs.F.min} method for details. *)
    val opt : ?lambda:float -> ?tolerance:float -> unit -> opt

    (** [coefficients t] the weights assigned to the log-odds of the features.

        The first coefficient is for a constant term and therefore
        there will always be 1 more than the [size] of features. *)
    val coefficients : t -> float array array

    (** [class_order t] specifies the order in which coefficients are returned. *)
    val class_order : t -> class_ list

  end
