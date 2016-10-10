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

open Oml_util

module Binomial(Data: Cls_intf.Dummy_encoded_data) = struct

  type feature = Data.feature
  type clas = Data.clas

  type samples = (clas * feature) list

  type opt =
    { smoothing : float
    ; bernoulli : bool
    }

  let opt ?(smoothing=0.) ?(bernoulli=false) () = { smoothing; bernoulli}
  let default = opt ()

  type t =
    (* Store the class prior in last element of the array. *)
    { table       : (clas * float array) list
    ; e_bernoulli : bool
    }

  let safe_encoding f =
    let e = Data.encoding f in
    if Array.any (fun i -> i >= Data.size) e then
      invalid_arg ~m:"Binomial" ~f:"encoding" "index beyond the encoding size."
    else
      e

  let eval nb b =
    let to_prior class_probs = class_probs.(Data.size) in
    let to_likelihood class_probs =
      let idx = safe_encoding b in
      if nb.e_bernoulli then
        let set = Array.to_list idx in
        Common_naive_bayes.prod_arr (fun i ->
          if List.mem i ~set then
            class_probs.(i)
          else
            (1.0 -. class_probs.(i)))
          (Array.init Data.size (fun x -> x))
      else
        Common_naive_bayes.prod_arr (fun i -> class_probs.(i)) idx
    in
    Common_naive_bayes.eval ~to_prior ~to_likelihood nb.table

  module Cm = Map.Make(struct type t = clas let compare = compare end)

  let estimate ?(opt=default) ?classes data =
    let aa = Data.size + 1 in
    let init _cls = Array.make aa 0 in
    let update arr ftr =
      let en = safe_encoding ftr in
      Array.iter (fun i -> arr.(i) <- arr.(i) + 1) en;
      (* keep track of the class count at the end of array. *)
      arr.(Data.size) <- arr.(Data.size) + 1;
      arr
    in
    let incorporate all num_classes totalf =
      let to_prob = Common_naive_bayes.smoothing_to_prob opt.smoothing in
      List.map all ~f:(fun (cl, attr_count) ->
        let prior_count = float attr_count.(Data.size) in
        let likelihood =
          Array.init aa (fun i ->
            if i = Data.size then    (* Store the prior at the end. *)
              to_prob prior_count totalf (float num_classes)
            else
              to_prob (float attr_count.(i)) prior_count 2.0)   (* Binary. *)
        in
        cl, likelihood)
    in
    let table =
      Common_naive_bayes.estimate "Binomial"
        init update incorporate (module Cm) ?classes data
    in
    {table ; e_bernoulli = opt.bernoulli}

  type feature_probability = float array

  let class_probabilities nb cls =
    let arr = List.assoc cls nb.table in
    let cls_p = arr.(Data.size) in
    let ftr_p = Array.sub arr 0 Data.size in
    (cls_p, fun ftr -> Array.map (fun i -> ftr_p.(i)) (Data.encoding ftr))

end (* Binomial *)

module Categorical(Data: Cls_intf.Category_encoded_data) = struct

  type feature = Data.feature
  type clas = Data.clas
  type samples = (clas * feature) list

  type t = (clas * (float * float array array)) list

  type opt = float
  let opt ?(smoothing=0.0) () = smoothing
  let default = opt ()

  let safe_encoding f =
    let e = Data.encoding f in
    let same_size = Array.length e = Array.length Data.encoding_sizes in
    let constrained =
      Array.map2 (fun v s -> v < s) e Data.encoding_sizes
      |> Array.all (fun x -> x)
    in
    if same_size && constrained then
      e
    else
      invalid_arg ~m:"Category_encoded_data" ~f:"encoding"
        "same size %b, constrained: %b" same_size constrained

  type feature_probability = float array

  let class_probabilities t cls =
    let (prior, likelihood_arr) = List.assoc cls t in
    prior,
    (fun ftr ->
      Array.map2 (fun i lk_arr -> lk_arr.(i)) (safe_encoding ftr) likelihood_arr)

  let eval table feature =
    let to_prior (prior, _) = prior in
    let to_likelihood (_, ftr_prob) =
      let indices = safe_encoding feature in
      Common_naive_bayes.prod_arr2 (fun i lk_arr -> lk_arr.(i)) indices ftr_prob
    in
    Common_naive_bayes.eval ~to_prior ~to_likelihood table

  module Cm = Map.Make(struct type t = clas let compare = compare end)

  let estimate ?(opt=default) =
    let init _ = (0, Array.map (fun i -> Array.make i 0) Data.encoding_sizes) in
    let update (c, arr) ftr =
      let ftr_arr = safe_encoding ftr in
      Array.iteri (fun i j -> arr.(i).(j) <- arr.(i).(j) + 1) ftr_arr;
      (c + 1, arr)
    in
    let incorporate all num_classes totalf =
      let to_prob = Common_naive_bayes.smoothing_to_prob opt in
      List.map all ~f:(fun (cl, (class_count, attr_count)) ->
        let prior      = to_prob (float class_count) totalf (float num_classes) in
        let likelihood =
          Array.map (fun arr ->
            let farr = Array.map float arr in
            let lsum = Array.sumf farr in
            let fssf = float (Array.length arr) in
            Array.map (fun c -> to_prob c lsum fssf) farr)
            attr_count
        in
        cl, (prior, likelihood))
    in
    Common_naive_bayes.estimate "Categorical" init update incorporate
      (module Cm)

  end (* Categorical *)
