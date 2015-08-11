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


(** The classifiers below assign a discrete probability distribution over the
    list of class 'a in their training set. *)
type 'a probabilities = ('a * float) list

(** [most_likely probabilities] returns the most likely class from the
    discrete probability distribution. *)
val most_likely : 'a probabilities -> 'a

(** A discrete Naive Bayes classifier of class ['cls] by observing
    features ['ftr]. *)
type ('cls, 'ftr) naive_bayes

(** [class_probabilities bayes class] returns the prior and per feature likelihood
    probabilities learned by [bayes] for [class].

    @raise Not_found if [bayes] never trained on [class]. *)
val class_probabilities : ('cls, 'ftr) naive_bayes -> 'cls -> float * float array

(** [estimate smoothing classes feature_size to_feature_array training_data]
    trains a discrete Naive Bayes classifier based on the [training_data].
    [to_feature_array] maps a feature to an integer array of indices of in
    the feature_space bounded by \[0,feature_size\). Optionally, [classes]
    supplies all the classes to learn or they're aggregated from observations
    in data (this is useful, for classes that may be 'missing' in the data and
    smoothing is applied.
    
    Additive [smoothing] can be applied to the final estimates if provided.
    When estimating a probability distribution by counting observed instances
    in the feature space we may want to smooth the values, particularly if our
    training data is sparse.

    [http://en.wikipedia.org/wiki/Additive_smoothing]

*)
val estimate : ?smoothing:float -> ?classes:'cls list ->
              feature_size:int -> ('ftr -> int array) -> ('cls * 'ftr) list ->
              ('cls, 'ftr) naive_bayes

(** [eval bernoulli classifier feature] classifies [feature]
    according to [classifier]. if [bernoulli] is specified we treat the
    underlying distribution as Bernoulli (as opposed to Multinomial) and
    estimate the likelihood with (1-p_i) for features [i] that are missing
    from [feature].
*)
val eval : ?bernoulli:bool -> ('cls, 'ftr) naive_bayes -> 'ftr -> 'cls probabilities

(** A discrete multinomial Naive Bayes classifier of class ['cls] by
    observing ['ftr], each 'cls represented by an integer array for each
    of it's categories. *)
type ('cls, 'ftr) naive_bayes_mv

(** Return the class probabilities of a multinomial naive bayes. *)
val class_probabilities_mv : ('cls, 'ftr) naive_bayes_mv -> 'cls -> ('ftr -> float * float array)

(** Estimate a multinomial naive bayes. *)
val estimate_mv : ?smoothing:float -> ?classes:'cls list ->
                  feature_sizes:int array -> ('ftr -> int array) ->
                  ('cls * 'ftr) list -> ('cls, 'ftr) naive_bayes_mv

(** Evaluate a new class instance using the trained multinomial naive bayes. *)
val eval_mv : ('cls, 'ftr) naive_bayes_mv  -> 'ftr -> 'cls probabilities


(** A continuous Gaussian Naive Bayes classifier of class ['cls]. The
    feature space is assumed to be a float array. *)
type 'cls gauss_bayes

(** [gauss_estimate training_data] trains a Gaussian Naive Bayes classifier from
    [training_data], where all of the data are of the same length; feature size.
    Optionally, [classes] supplies all the classes to learn or they're
    aggregated from observations in data. *)
val gauss_estimate : ?classes:'cls list ->
                     ('cls * float array) list -> 'cls gauss_bayes

(** [gauss_eval classifier feature] classify the [feature] using the [classifier]. *)
val gauss_eval : 'cls gauss_bayes -> float array -> 'cls probabilities

(** A logistic regression classifier. *)
type 'cls log_reg

(** Evaluate a logistic regression classifier. *)
val log_reg_eval : 'cls log_reg -> float array -> 'cls probabilities

(** Estimate a logistic regression classifier. *)
val log_reg_estimate : class_f:('cls -> bool) ->
                       ('cls * float array) list -> 'cls log_reg

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

