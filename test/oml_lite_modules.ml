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

module Rank = Rank
module Util = Util

(* Uncategorized. *)
module Estimations = Estimations
module Solvers = Solvers
module Matrices = Matrices
module Vectors = Vectors

(* Statistics. *)
module Descriptive = Descriptive
module Measures = Measures
module Sampling = Sampling
module Oml_lite_functions = Oml_lite_functions

(* Online *)
module Online = Online

(* Classification. *)
module Oml_lite_naive_bayes = Oml_lite_naive_bayes

(* Regression. *)
module Oml_lite_univariate = Oml_lite_univariate
