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

open Test_utils

module Rank = Rank
(*
module Util_t = Util_t

module Estimations_t = Estimations_t
module Functions_t = Functions_t
module Solvers_t = Solvers_t
module Svd_t = Svd_t

module Matrices_t = Matrices_t
module Vectors_t = Vectors_t

module Descriptive_t = Descriptive_t
module Distributions_t = Distributions_t
module Hypothesis_test_t = Hypothesis_test_t
module Measures_t = Measures_t
module Sampling_t = Sampling_t

module Online_t = Online_t

module Classification_t = Classification_t

module Interpolate_t = Interpolate_t
module Regression_t = Regression_t

module Pca_t = Pca_t
*)

let () =
  if Array.length Sys.argv > 1 then
    Test.launch_test Sys.argv.(1)
  else
    Test.launch_tests ();
