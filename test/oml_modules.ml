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

(* Reference all of the modules that are found exclusively in Oml_lite.
  These should not have any C/Fortran dependencies. The associated tests will
  be run when this module is referenced in Oml_lite_test. *)

module Oml_rank = Oml_rank
module Oml_util = Oml_util

(* Uncategorized. *)
module Oml_estimations = Oml_estimations
module Oml_solvers = Oml_solvers
module Oml_matrices = Oml_matrices
module Oml_vectors = Oml_vectors

(* Statistics. *)
module Oml_descriptive = Oml_descriptive
module Oml_measures = Oml_measures
module Oml_sampling = Oml_sampling
module Oml_functions = Oml_functions

(* Online *)
module Oml_online = Oml_online

(* Classification. *)
module Oml_naive_bayes = Oml_naive_bayes

(* Regression. *)
module Oml_univariate = Oml_univariate
