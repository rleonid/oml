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

(* Just referencing the modules will load the tests. *)
module Rank = Rank
module Util = Util

module Estimations = Estimations
module Solvers = Solvers
module Svd = Svd

module Matrices = Matrices
module Vectors = Vectors

module Functions = Functions
module Descriptive = Descriptive
module Distributions = Distributions
module Hypothesis_test = Hypothesis_test
module Measures = Measures
module Sampling = Sampling
module Online = Online


module Naive_bayes = Naive_bayes
module Logistic_regression = Logistic_regression

module Univariate = Univariate
module Multivariate = Multivariate
module Tikhonov = Tikhonov
module Interpolate = Interpolate

module Pca = Pca

let () =
  if Array.length Sys.argv > 1 then
    Test.launch_test Sys.argv.(1)
  else
    Test.launch_tests ();
