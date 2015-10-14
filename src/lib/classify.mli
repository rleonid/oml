(*
   Copyright 2015:
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

open Util

(** The classifiers below assign a discrete probability distribution over the
    list of class 'a in their training set. *)
type 'a probabilities = ('a * float) list

(** [most_likely probabilities] returns the most likely class from the
    discrete probability distribution.

    @raise Invalid_argument if [probabilities] is empty. *)
val most_likely : 'a probabilities -> 'a

(** From the perspective of classification there are two types of data:
  {{!clas}classes} and {{!feature}features}. *)
module type Data_intf = sig
  type clas       (** Classes that represent groups we want to classify. *)
  type feature    (** Features that describe elements of a class.*)
end

(** TODO Encoding data. *)

(** Data where features are encoded using
    {{:https://en.wikipedia.org/wiki/Dummy_variable_%28statistics%29}Dummy Variables}
    (aka {{:https://en.wikipedia.org/wiki/One-hot}one-hot encoding}).

    Each [feature] is described using an
    array of binary variables of length
    {{!val:size} size}. *)
module type Dummy_encoded_data_intf = sig
  include Data_intf

  (** [encoding f] returns indices into an array. If [i] is in [encoding f]
      that indicates the {b presence} of dummy variable [i] in [f]. *)
  val encoding : feature -> int array

  (** The number of dummy variables to track. *)
  val size : int
end

(** Data where features are encoded using a set of
    {{:https://en.wikipedia.org/wiki/Categorical_variable}Category variables}.

    Each [feature] is described using an array of category variables
    constrained by {{!val:encoding_sizes} encoding_sizes}. *)
module type Category_encoded_data_intf = sig
  include Data_intf

  (** [encoding f] returns an array where each element [e] at position [i]
      describes the category value [e] of the [i]th category variables.

      @raise Invalid_argument if the returned array is not of the same length
      as [encoding_sizes] or if the [i]th element takes a greater than or
      equal to the [i]th sizes in [encoding_sizes]. *)
  val encoding : feature -> int array

  (** [encoding_sizes] the number of categories for each variable. *)
  val encoding_sizes : int array
end

(** Data where features are encoded using a set of quantitative variables.

    Each [feature] is described using an array of quantitative variables of
    length {{!val:size} size}. In many cases this is the simplest encoding. *)
module type Continuous_encoded_data_intf = sig
  include Data_intf

  (** [encoding f] returns an array where each element [e] at position [i]
      describes the [i]th variable with value [e].

      @raise Invalid_argument if the returned array does not equal
      {{!val:size} size}.*)
  val encoding : feature -> float array

  (** The number of quantitative variables needed to describe the features.*)
  val size : int
end

(** A {{!type:t}classifier}, once {{!val:estimate} estimated} from
    the {{!type:Classify.Data_intf.feature}features} found in
    {{!type:samples}samples} of data, assigns
    {{!probabilities}probabilities} to
    {{!type:Classify.Data_intf.clas}classes}
    on future samples when {{!val:eval}evaluated}. *)
module type Classifier_intf = sig
  include Data_intf
  include Util.Optional_arg_intf


  (** The classifier. *)
  type t

  (** [eval classifier feature] assign {{!probabilities}probabilities} to the
      possible {{!type:Classify.Data_intf.clas}classes} based upon [feature]. *)
  val eval : t -> feature -> clas probabilities

  (** Representing training data. *)
  type samples = (clas * feature) list

  (** [estimate spec classes samples] estimates a classifier based upon the
      training [samples].

      [classes] is an optional argument to specify ahead of time the possible
      classes to train on (defaults to the ones found in the training data).
      This is useful for models where we know the population domain but may
      not see an example of a training datum for rare cases.

      [spec] are the optional classifier dependent estimation/evaluation
      arguments.

      @raise Invalid_argument if [classes] are specified and new ones are
      found in the training [samples].
      @raise Invalid_argument if [samples] is empty.
  *)
  val estimate : ?spec:spec -> ?classes:clas list -> samples -> t
end

(** A generative classifier builds models of the form
    P({{!type:Classify.Data_intf.clas}class},
      {{!type:Classify.Data_intf.feature}feature}).

    For current purposes these classifiers can return individual probabilities
    of the form P({{!type:Classify.Data_intf.feature}feature} |
      {{!type:Classify.Data_intf.clas}class}).
*)
module type Generative_intf = sig
  include Classifier_intf

  (** [class_probabilities t class] returns the prior and per feature
      likelihood probabilities learned by [t] for [class].

      @raise Not_found if [t] never trained on [class]. *)
  (* TODO: refactor the end type here to be something like feature pdf type *)
  val class_probabilities : t -> clas -> float * (feature -> float array)

end

(** {{:http://en.wikipedia.org/wiki/Additive_smoothing}Additive smoothing}
    can be applied to the final estimate of Naive Bayes classifiers.

    When estimating a probability distribution by counting observed instances
    in the feature space we may want to smooth the values, particularly if our
    training data is sparse. *)
type smoothing = float

(** The optional configuration of a
    {{!module:BinomialNaiveBayes}Binomial Naive Bayes} classifier.

  If [bernoulli] is specified we treat the underlying distribution as Bernoulli
  (as opposed to Multinomial) and estimate the likelihood with (1-p_i) for
  features [i] that are missing from a feature when
  {{!val:Classifier_intf.eval}evaluated}. *)
type binomial_spec =
  { smoothing : smoothing
  ; bernoulli : bool
  }

(** Train a
  {{:https://en.wikipedia.org/wiki/Naive_Bayes_classifier}Naive Bayes}
  classifier on data encoded using
  {{!modtype:Dummy_encoded_data_intf}Dummy variables.} *)
module BinomialNaiveBayes(D: Dummy_encoded_data_intf) :
  Generative_intf with type spec = binomial_spec
                  and type feature = D.feature
                  and type clas = D.clas

(** Train a
  {{:https://en.wikipedia.org/wiki/Naive_Bayes_classifier}Naive Bayes}
  classifier on data encoded using
  {{!modtype:Category_encoded_data_intf}Categorical variables.} *)
module CategoricalNaiveBayes(D: Category_encoded_data_intf) :
  Generative_intf with type spec = smoothing
                  and type feature = D.feature
                  and type clas = D.clas

(** Train a
  {{:https://en.wikipedia.org/wiki/Naive_Bayes_classifier#Gaussian_naive_Bayes}
  Gaussian Naive Bayes} by estimating mean and standard deviations
  for each of the quantitative features in the
  {{!modtype:Continuous_encoded_data_intf}encoded data}. *)
module GaussianNaiveBayes(D: Continuous_encoded_data_intf) :
  Generative_intf with type spec = unit
                  and type feature = D.feature
                  and type clas = D.clas

(** Use
  {{:https://en.wikipedia.org/wiki/Logistic_regression}
  Logistic Regression} to estimate log-odds
  for each of the quantitative features in the
  {{!modtype:Continuous_encoded_data_intf}encoded data},
  per class*)
module LogisticRegression(D: Continuous_encoded_data_intf) :
  Classifier_intf with type spec = unit
                  and type feature = D.feature
                  and type clas = D.clas

(** A two class prediction. *)
type binary =
  { predicted   : bool
  ; probability : float   (* Probability of the _predicted_ class. *)
  ; actual      : bool
  }

(** Common statistics that describe performance of a two state classifier. *)
type descriptive_statistics =
  { sensitivity         : float
  ; specificity         : float
  ; positive_predictive : float
  ; negative_predictive : float
  ; accuracy            : float
  ; area_under_curve    : float   (* Area under ROC. *)
  }

(* For a classifier that returns associated probabilities,
   describe it's performance. *)
val evaluate_performance : binary list -> descriptive_statistics

(* For a list of false positive rates and true positive rates, estimate the
   AUC by trapezoid integration. End points are added by default. *)
val cross_validated_auc : (float * float) array -> float
