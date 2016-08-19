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

open Util
module List = ListLabels
module O = Online
open Intf

let multiply_ref = ref true

(* How we choose to multiply large arrays of small probabilities.
  TODO: develop a smart heuristic for switching to log transformed addition. *)
let prod_arr, prod_arr2 =
  if !multiply_ref then
    (fun f x -> Array.fold_left (fun p x -> p *. f x) 1.0 x),
    (fun f x y -> Array.fold2 (fun p x y -> p *. f x y) 1.0 x y)
  else
    (fun f x -> Array.fold_left (fun s x -> s +. log (f x)) 0.0 x |> exp),
    (fun f x y -> Array.fold2 (fun s x y -> s +. log (f x y)) 0.0 x y |> exp)

let within a b x = max a (min x b)

let smoothing_to_prob = function
  | 0.0    ->
      (fun count bkgrnd _ -> count /. bkgrnd)
  | sf ->
      let sf = within 0.0 1.0 sf in
      (fun count bkgrnd space_size ->
        (count +. sf) /. (bkgrnd +. sf *. space_size))

(* likelihood refers to the conditional probability of observing a feature f_i
for class C_k : P(f_i|C_k) *)

(* Simplify the evaluate code by keeping track of the evidence.*)
let eval_naive_bayes ~to_prior ~to_likelihood cls_assoc =
  let evidence = ref 0.0 in
  List.map cls_assoc ~f:(fun (c, e) ->
    let prior = to_prior e in
    let likelihood = to_likelihood e in
    let prob = prior *. likelihood in
    evidence := !evidence +. prob;
    (c, prob))
  |> List.map ~f:(fun (c, p) -> (c, p /. !evidence))

(* init - init per class data
   update - update per class data
   incorporate - convert class assoc to final shape *)
let estimate_naive_bayes m (type c) init update incorporate
  (module Cm : Map.S with type key = c) ?(classes=[]) (data : (c * 'a) list) =
  let ia fmt = invalid_arg ~m ~f:"estimate" fmt in
  if data = [] then
    ia "Nothing to train on"
  else
    let error_on_new = classes <> [] in
    let init_map =
      List.fold_left classes
        ~f:(fun m c -> Cm.add c (init c) m)
        ~init:Cm.empty
    in
    let total, first_pass =
      List.fold_left data
        ~f:(fun (total, m) (cls, ftr) ->
          let m' =
            try
              let a = Cm.find cls m in
              Cm.add cls (update a ftr) m
            with Not_found ->
              if error_on_new then
                (*ia (Printf.sprintf "Unexpected class at datum %d" total*)
                ia "Unexpected class at datum %d" total
              else
                Cm.add cls (update (init cls) ftr) m
          in
          (total + 1, m'))
        ~init:(0, init_map)
    in
    let num_classes = Cm.cardinal first_pass in
    incorporate (Cm.bindings first_pass) num_classes (float total)

module Binomial(Data: Dummy_encoded_data) = struct

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
        prod_arr (fun i ->
          if List.mem i ~set then
            class_probs.(i)
          else
            (1.0 -. class_probs.(i)))
          (Array.init Data.size (fun x -> x))
      else
        prod_arr (fun i -> class_probs.(i)) idx
    in
    eval_naive_bayes ~to_prior ~to_likelihood nb.table

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
      let to_prob = smoothing_to_prob opt.smoothing in
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
      estimate_naive_bayes "Binomial"
        init update incorporate (module Cm) ?classes data
    in
    {table ; e_bernoulli = opt.bernoulli}

  type feature_probability = float array

  let class_probabilities nb cls =
    let arr = List.assoc cls nb.table in
    let cls_p = arr.(Data.size) in
    let ftr_p = Array.sub arr 0 Data.size in
    (cls_p, fun ftr -> Array.map (fun i -> ftr_p.(i)) (Data.encoding ftr))

end

module Categorical(Data: Category_encoded_data) = struct

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
      prod_arr2 (fun i lk_arr -> lk_arr.(i)) indices ftr_prob
    in
    eval_naive_bayes ~to_prior ~to_likelihood table

  module Cm = Map.Make(struct type t = clas let compare = compare end)

  let estimate ?(opt=default) =
    let init _ = (0, Array.map (fun i -> Array.make i 0) Data.encoding_sizes) in
    let update (c, arr) ftr =
      let ftr_arr = safe_encoding ftr in
      Array.iteri (fun i j -> arr.(i).(j) <- arr.(i).(j) + 1) ftr_arr;
      (c + 1, arr)
    in
    let incorporate all num_classes totalf =
      let to_prob = smoothing_to_prob opt in
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
    estimate_naive_bayes "Categorical" init update incorporate
      (module Cm)

  end

#ifndef OML_LITE
module Gaussian(Data: Continuous_encoded_data) = struct

  module D = Statistics.Distributions
  (*type feature := Data.feature
  type clas := Data.clas *)

  type samples = (Data.clas * Data.feature) list

  type t = (Data.clas * (float * (float * float) array)) list

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
      prod_arr2 (fun (mean,std) y ->
        if std = 0. then 1. else D.normal_pdf ~mean ~std y)
        lkhd indices
    in
    eval_naive_bayes ~to_prior ~to_likelihood table

  type opt = unit
  let default = ()

  module Cm = Map.Make(struct type t = Data.clas let compare = compare end)

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
      rs_lst
      |> List.map ~f:(fun (c, (cf, rsarr)) ->
          let class_prior = (float cf) /. totalf in
          let attr_params = Array.map select rsarr in
          (c, (class_prior, attr_params)))
    in
    estimate_naive_bayes "Gaussian"
      init update incorporate (module Cm)

  end
#endif

