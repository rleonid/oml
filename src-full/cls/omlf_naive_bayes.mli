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

include module type of Oml.Classification.Naive_bayes

(** Train a
  {{:https://en.wikipedia.org/wiki/Naive_Bayes_classifier#Gaussian_naive_Bayes}
  Gaussian Naive Bayes} by estimating mean and standard deviations
  for each of the quantitative features in the
  {{!modtype:Cls_intf.Continuous_encoded_data}encoded data}. *)
module Gaussian(D: Oml.Classification.Interfaces.Continuous_encoded_data) : sig
  include Oml.Classification.Interfaces.Generative
      with type feature := D.feature
       and type class_ := D.class_
       and type feature_probability = float array
end
