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
