(*
   Copyright 2015:2016
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

include Oml_common_univariate

let confidence_interval, prediction_interval =
  let interval a lrm ~alpha x =
    let dgf = lrm.size -. 2.0 in
    let degrees_of_freedom = truncate dgf in
    let t  =
      Omlf_distributions.student_quantile ~degrees_of_freedom (alpha /. 2.0)
    in
    let b  = (x -. lrm.m_pred) ** 2.0 /. lrm.s_xx in
    let c  = lrm.sum_residuals /. dgf in
    let se = sqrt ((a +. b) *. c) in
    let d  = t *. se in
    let y  = eval lrm x in
    (y -. d), (y +. d)
  in
  (fun lrm -> interval (1.0 /. lrm.size) lrm),
  (fun lrm -> interval ((lrm.size +. 1.0) /. lrm.size) lrm)

let alpha_test ?(null=0.0) t =
  let alpha_var =
    t.inferred_var *. (1. /. t.sum_weights +.
    t.m_pred *. t.m_pred /. t.s_xx)
  in
  let degrees_of_freedom = truncate t.size - 2 in
  let diff = t.alpha -. null in
  Omlf_hypothesis_test.(t_test
                          Two_sided ~degrees_of_freedom ~diff
                          ~error:(sqrt alpha_var))

let beta_test ?(null=0.0) t =
  let beta_var = t.inferred_var /. t.s_xx in
  let degrees_of_freedom = truncate t.size - 2 in
  let diff = t.beta -. null in
  Omlf_hypothesis_test.(t_test
                          Two_sided ~degrees_of_freedom ~diff
                          ~error:(sqrt beta_var))

let coefficient_tests ?null t =
  [| alpha_test ?null t ; beta_test ?null t |]
