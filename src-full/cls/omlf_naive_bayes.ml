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

include Oml_naive_bayes
open Oml_util

module Gaussian(Data: Oml_classification_interfaces.Continuous_encoded_data) = struct

  module D = Omlf_distributions
  module O = Oml_online

  type samples = (Data.class_ * Data.feature) list

  type t = (Data.class_ * (float * (float * float) array)) list

  let safe_encoding f =
    let e = Data.encoding f in
    let l = Array.length e in
    if l = Data.size then
      e
    else
      invalid_arg ~m:"Continuous_encoded_data" ~f:"encoding"
        "size %d actual %d" Data.size l

  type feature_probability = float array

  let class_probabilities t cls =
    let (prior, dist_params) = List.assoc cls t in
    prior,
    (fun ftr ->
      Array.map2 (fun (mean,std) y ->
        (* if std = 0. then nan is ok, signals error. *)
        D.normal_pdf ~mean ~std y)
        dist_params (safe_encoding ftr))

  let eval table feature =
    let to_prior (prior, _) = prior in
    let to_likelihood (_, lkhd) =
      let indices = safe_encoding feature in
      Oml_common_naive_bayes.prod_arr2 (fun (mean,std) y ->
        if std = 0. then 1. else D.normal_pdf ~mean ~std y)
        lkhd indices
    in
    Oml_common_naive_bayes.eval ~to_prior ~to_likelihood table

  type opt = unit
  let default = ()

  module Cm = Map.Make(struct type t = Data.class_ let compare = compare end)

  let estimate ?(opt=default) =
    ignore opt;
    let init _c = (0, Array.make Data.size O.empty) in
    let update (c, rs_arr) ftr =
      let attr = safe_encoding ftr in
      (c + 1, Array.map2 O.update rs_arr attr)
    in
    let incorporate rs_lst _ totalf =
      (* A lot of the literature in estimating Naive Bayes focuses on estimating
         the parameters using Maximum Likelihood. The Online estimate of variance
         computes the unbiased form. Not certain if we should implement the
         n/(n-1) conversion below. *)
      let select rs = rs.O.mean, (sqrt rs.O.var) in
      List.map rs_lst ~f:(fun (c, (cf, rsarr)) ->
        let class_prior = (float cf) /. totalf in
        let attr_params = Array.map select rsarr in
        (c, (class_prior, attr_params)))
    in
    Oml_common_naive_bayes.estimate "Gaussian"
      init update incorporate (module Cm)

end (* Gaussian *)
