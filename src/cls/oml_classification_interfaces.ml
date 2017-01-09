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

module Probabilities = Oml_probabilities

(** Interfaces to categorize the different logic of classification. *)

(** From the perspective of classification there are two types of data:
  {{!class_}classes} and {{!feature}features}. *)
module type Data = sig
  type class_       (** Classes that represent groups we want to classify. *)
  type feature    (** Features that describe elements of a class.*)
end

(** Data where features are encoded using
    {{:https://en.wikipedia.org/wiki/Dummy_variable_%28statistics%29}Dummy Variables}
    (aka {{:https://en.wikipedia.org/wiki/One-hot}one-hot encoding}).

    Each [feature] is described using an
    array of binary variables of length
    {{!val:size} size}. *)
module type Dummy_encoded_data = sig
  include Data

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
module type Category_encoded_data = sig
  include Data

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
module type Continuous_encoded_data = sig
  include Data

  (** [encoding f] returns an array where each element [e] at position [i]
      describes the [i]th variable with value [e].

      @raise Invalid_argument if the returned array does not equal
      {{!val:size} size}.*)
  val encoding : feature -> float array

  (** The number of quantitative variables needed to describe the features.*)
  val size : int
end

(** A {{!type:t}classifier}, once {{!val:estimate} estimated} from
    the {{!type:Data.feature}features} found in
    {{!type:samples}samples} of data, assigns
    {{!Probabilities.t}probabilities} to
    {{!type:Data.class_}classes}
    on future samples when {{!val:eval}evaluated}. *)
module type Classifier = sig
  include Data
  include Oml_util.Optional_arg_intf

  (** The classifier. *)
  type t

  (** [eval classifier feature] assign {{!Probabilities.t}probabilities} to the
      possible {{!type:Data.class_}classes} based upon [feature]. *)
  val eval : t -> feature -> class_ Probabilities.t

  (** Representing training data. *)
  type samples = (class_ * feature) list

  (** [estimate opt classes samples] estimates a classifier based upon the
      training [samples].

      [classes] is an optional argument to specify ahead of time the possible
      classes to train on (defaults to the ones found in the training data).
      This is useful for models where we know the population domain but may
      not see an example of a training datum for rare cases.

      [opt] are the optional classifier dependent estimation/evaluation
      arguments.

      @raise Invalid_argument if [classes] are specified and new ones are
      found in the training [samples].
      @raise Invalid_argument if [samples] is empty.
  *)
  val estimate : ?opt:opt -> ?classes:class_ list -> samples -> t
end

(** A generative classifier builds models of the form
    P({{!type:Data.class_}class},
      {{!type:Data.feature}feature}).

    For current purposes these classifiers can return individual probabilities
    of the form P({{!type:Data.feature}feature} |
      {{!type:Data.class_}class}).
*)
module type Generative = sig
  include Classifier

  type feature_probability

  (** [class_probabilities t class] returns the prior and per feature
      likelihood probability (ies) learned by [t] for [class].

      @raise Not_found if [t] never trained on [class]. *)
  val class_probabilities : t -> class_ -> float * (feature -> feature_probability)

end
