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

(* Reference all of the modules that are exclusively in Oml; the ones with
   C/Fortran dependencies. This module is also referenced in the full test
   driver: oml_test.ml. *)

(* Uncategorized. *)
module Svd = Svd

(* Statistics. *)
module Oml_functions = Oml_functions
module Distributions = Distributions
module Hypothesis_test = Hypothesis_test

(* Classification. *)
module Logistic_regression = Logistic_regression
module Oml_naive_bayes = Oml_naive_bayes

(* Regression. *)
module Multivariate = Multivariate
module Tikhonov = Tikhonov

(* Unsupervised. *)
module Pca = Pca
