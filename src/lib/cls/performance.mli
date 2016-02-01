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

(** Evaluating classification performance. *)

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
