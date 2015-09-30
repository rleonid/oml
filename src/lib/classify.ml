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

module List = ListLabels
open Util

type 'a probabilities = ('a * float) list

let most_likely = function
  | []    -> invalidArg "Classify.most_likely: empty probabilities"
  | h::tl ->
    List.fold_left ~f:(fun ((_,p1) as c1) ((_,p2) as c2) ->
      if p2 > p1 then c2 else c1) ~init:h tl
    |> fst

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

module type Data_intf = sig
  type clas
  type feature
end

module type Dummy_encoded_data_intf = sig
  include Data_intf

  val encoding : feature -> int array
  val size : int
end

module type Estimate_optional_arg_intf = sig
  type spec
  val default : spec
end

module type Classifier_intf = sig
  include Data_intf
  include Estimate_optional_arg_intf

  type t

  val eval : t -> feature -> clas probabilities

  type samples = (clas * feature) list
  val estimate : ?spec:spec -> ?classes:clas list -> samples -> t
end

module type Generative_intf = sig
  include Classifier_intf

  val class_probabilities : t -> clas -> float * (feature -> float array)

end

let within a b x = max a (min x b)

let smoothing_to_prob = function
  | 0.0    ->
      (fun count bkgrnd _ -> count /. bkgrnd)
  | sf ->
      let sf = within 0.0 1.0 sf in
      (fun count bkgrnd space_size ->
        (count +. sf) /. (bkgrnd +. sf *. space_size))

type binomial_spec =
  { smoothing : float
  ; bernoulli : bool
  }

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
let estimate_naive_bayes modulename (type c) init update incorporate
  (module Cm : Map.S with type key = c) ?(classes=[]) (data : (c * 'a) list) =
  let ia = invalidArg "Classify.%s.estimate: %s" modulename in
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
                ia (Printf.sprintf "Unexpected class at datum %d" total)
              else
                Cm.add cls (update (init cls) ftr) m
          in
          (total + 1, m'))
        ~init:(0, init_map)
    in
    let num_classes = Cm.cardinal first_pass in
    incorporate (Cm.bindings first_pass) num_classes (float total)

module BinomialNaiveBayes(Data: Dummy_encoded_data_intf)
  : (Generative_intf with type feature = Data.feature
                     and type clas = Data.clas
                     and type spec = binomial_spec)
  = struct

  type feature = Data.feature
  type clas = Data.clas

  type samples = (clas * feature) list

  type t =
    (* Store the class prior in last element of the array. *)
    { table       : (clas * float array) list
    ; e_bernoulli : bool
    }

  let safe_encoding f =
    let e = Data.encoding f in
    if Array.any (fun i -> i >= Data.size) e then
      invalidArg "BinomialNaiveBayes.encoding: index beyond the encoding size."
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
    eval_naive_bayes to_prior to_likelihood nb.table

  type spec = binomial_spec
  let default = { smoothing = 0.0; bernoulli = false }

  module Cm = Map.Make(struct type t = clas let compare = compare end)

  let estimate ?(spec=default) ?classes data =
    let aa = Data.size + 1 in
    let init cls = Array.make aa 0 in
    let update arr ftr =
      let en = safe_encoding ftr in
      Array.iter (fun i -> arr.(i) <- arr.(i) + 1) en;
      (* keep track of the class count at the end of array. *)
      arr.(Data.size) <- arr.(Data.size) + 1;
      arr
    in
    let incorporate all num_classes totalf =
      let to_prob = smoothing_to_prob spec.smoothing in
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
      estimate_naive_bayes "BinomialNaiveBayes"
        init update incorporate (module Cm) ?classes data
    in
    {table ; e_bernoulli = spec.bernoulli}

  let class_probabilities nb cls =
    let arr = List.assoc cls nb.table in
    let cls_p = arr.(Data.size) in
    let ftr_p = Array.sub arr 0 Data.size in
    (cls_p, fun ftr -> Array.map (fun i -> ftr_p.(i)) (Data.encoding ftr))

end

let assoc_opt ~default f lst =
  try
    let g = List.assoc f lst in
    let r = List.remove_assoc f lst in
    g, r
  with Not_found ->
    default (), lst

module type Category_encoded_data_intf = sig
  include Data_intf
  val encoding : feature -> int array
  val encoding_sizes : int array
end

type smoothing = float

module CategoricalNaiveBayes(Data: Category_encoded_data_intf)
  : (Generative_intf with type feature = Data.feature
                     and type clas = Data.clas
                     and type spec = smoothing)

  = struct

  type feature = Data.feature
  type clas = Data.clas

  type samples = (clas * feature) list

  type t = (clas * (float * float array array)) list

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
      invalidArg
        "Category_encoded_data_intf.encoding: same size %b, constrained: %b"
          same_size constrained

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
    eval_naive_bayes to_prior to_likelihood table

  type spec = smoothing
  let default = 0.0

  module Cm = Map.Make(struct type t = clas let compare = compare end)

  let estimate ?(spec=default) =
    let init _ = (0, Array.map (fun i -> Array.make i 0) Data.encoding_sizes) in
    let update (c, arr) ftr =
      let ftr_arr = safe_encoding ftr in
      Array.iteri (fun i j -> arr.(i).(j) <- arr.(i).(j) + 1) ftr_arr;
      (c + 1, arr)
    in
    let incorporate all num_classes totalf =
      let to_prob = smoothing_to_prob spec in
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
    estimate_naive_bayes "CategoricalNaiveBayes" init update incorporate
      (module Cm)

  end

module type Continuous_encoded_data_intf = sig
  include Data_intf
  val encoding : feature -> float array
  val size : int
end

let to_safe_encoding_size_checked interfacename size encoding f =
  let e = encoding f in
  let l = Array.length e in
  if l = size then
    e
  else
    invalidArg "%s.encoding: size %d actual %d" interfacename size l

module GaussianNaiveBayes(Data: Continuous_encoded_data_intf)
  : (Generative_intf with type feature = Data.feature
                     and type clas = Data.clas
                     and type spec = unit)

  = struct

  type feature = Data.feature
  type clas = Data.clas

  type samples = (clas * feature) list

  type t = (clas * (float * (float * float) array)) list

  let safe_encoding =
    to_safe_encoding_size_checked "Continuous_encoded_data_intf"
      Data.size Data.encoding

  let class_probabilities t cls =
    let (prior, dist_params) = List.assoc cls t in
    prior,
    (fun ftr ->
      Array.map2 (fun (mean,std) y -> Distributions.normal_pdf ~mean ~std y)
        dist_params (safe_encoding ftr))

  let eval table feature =
    let to_prior (prior, _) = prior in
    let to_likelihood (_, lkhd) =
      let indices = safe_encoding feature in
      prod_arr2 (fun (mean,std) y -> Distributions.normal_pdf ~mean ~std y)
        lkhd indices
    in
    eval_naive_bayes to_prior to_likelihood table

  type spec = unit
  let default = ()

  module Cm = Map.Make(struct type t = clas let compare = compare end)

  let estimate ?(spec=default) =
    let init c = (0, Array.make Data.size Running.empty) in
    let update (c, rs_arr) ftr =
      let attr = safe_encoding ftr in
      (c + 1, Array.map2 Running.update rs_arr attr)
    in
    let incorporate rs_lst _ totalf =
      (* A lot of the literature in estimating Naive Bayes focuses on estimating
         the parameters using Maximum Likelihood. The Running estimate of variance
         computes the unbiased form. Not certain if we should implement the
         n/(n-1) conversion below. *)
      let select rs = rs.Running.mean, (sqrt rs.Running.var) in
      rs_lst
      |> List.map ~f:(fun (c, (cf, rsarr)) ->
          let class_prior = (float cf) /. totalf in
          let attr_params = Array.map select rsarr in
          (c, (class_prior, attr_params)))
    in
    estimate_naive_bayes "GaussianNaiveBayes"
      init update incorporate (module Cm)

  end

module LogisticRegression(Data: Continuous_encoded_data_intf)
  : (Classifier_intf with type feature = Data.feature
                     and type clas = Data.clas
                     and type spec = unit)

  = struct

  open Lacaml_D

  module Log_reg = struct

    (* Code modified from
      http://math.umons.ac.be/anum/fr/software/OCaml/Logistic_Regression/ *)

    (* [logistic_grad_n_eval] returns the value of the function to maximize
       and store its gradient in [g]. *)
    let logistic_grad_n_eval ~lambda x y =
      (fun w g ->
        let s = ref 0. in
        ignore(copy ~y:g w);                   (* g ← w *)
        scal (-. lambda) g;                    (* g = -λ w *)
        for i = 0 to Array.length x - 1 do
          let yi = y.(i) in
          let e  = exp(-. yi *. dot w x.(i)) in
          s := !s +. log1p e;
          axpy x.(i) ~alpha:(yi *. e /. (1. +. e)) g;
        done;
        -. !s -. 0.5 *. lambda *. dot w w)

    (* TODO. expose lambda as possible [spec]. *)
    let log_reg ?(lambda=0.1) x y =
      let w = Vec.make0 (Vec.dim x.(0)) in
      ignore(Lbfgs.F.max (*~print:(Lbfgs.Every 10) *)
              (logistic_grad_n_eval ~lambda x y) w);
      w

  end

  type feature = Data.feature
  type clas = Data.clas

  type samples = (clas * feature) list

  type t =
    { weights : vec
    ; classes : (clas * float) list
    }

  let safe_encoding =
    to_safe_encoding_size_checked "Continuous_encoded_data_intf"
      Data.size Data.encoding

  let proba w x y = Float.(1. / (1. + exp(-. y * dot w x)))

  let eval lr feature =
    let a = safe_encoding feature in
    let m = Vec.init (Data.size + 1) (function | 1 -> 1.0 | i -> a.(i - 2)) in
    List.map (fun (c,c_i) -> c, proba lr.weights m c_i) lr.classes

  type spec = unit
  let default = ()

  let estimate ?(spec=default) ?(classes=[]) data =
    if data = [] then
      invalidArg "Classify.log_reg_estimate: Nothing to train on!"
    else
      let error_on_new = classes <> [] in
      (* convert the classes to index markers.
         TODO: are there better choices for these? literature,practice*)
      let itce i = float (2 * i - 1) in
      let assigned_cls_assoc =
        ref (List.mapi ~f:(fun i c -> c, itce i) classes)
      in
      let classes =
        List.mapi data ~f:(fun idx (c, _) ->
          try
            List.assoc c !assigned_cls_assoc
          with Not_found ->
            if error_on_new then
              invalidArg "Found a new (unexpected) class at datum %d" idx
            else
              let n = List.length !assigned_cls_assoc in
              let i = itce n in
              assigned_cls_assoc := (c, i) :: !assigned_cls_assoc;
              i)
        |> Array.of_list
      in
      if List.length !assigned_cls_assoc < 2 then
        invalidArg "Trying to estimate Log Reg on less than 2 classes."
      else
        (*let fs      = Array.length (snd (List.hd data)) in *)
        (* Fortran destination so 1 based. *)
        let to_f a  = Vec.init (Data.size + 1) (function | 1 -> 1.0 | i -> a.(i - 2)) in
        let ftrs    = List.map (fun (_, f) -> to_f (safe_encoding f)) data
                      |> Array.of_list
        in
        let weights = Log_reg.log_reg ftrs classes in
        { weights
        ; classes = !assigned_cls_assoc
        }

  end

type binary =
  { predicted   : bool
  ; probability : float
  ; actual      : bool
  }

type descriptive_statistics =
  { sensitivity         : float
  ; specificity         : float
  ; positive_predictive : float
  ; negative_predictive : float
  ; accuracy            : float
  ; area_under_curve    : float
  }

module BinaryClassificationPerformance = struct

  type t =
    | True_positive
    | False_negative
    | False_positive
    | True_negative

  let datum_to_t d =
    match d.actual, d.predicted with
    | true, true    -> True_positive
    | true, false   -> False_negative
    | false, true   -> False_positive
    | false, false  -> True_negative

  type classification_record =
    { true_positive   : int
    ; false_negative  : int
    ; false_positive  : int
    ; true_negative   : int
    }

  let empty_cr =
    { true_positive   = 0
    ; false_negative  = 0
    ; false_positive  = 0
    ; true_negative   = 0
    }

  let update_classification_record cr d =
    match datum_to_t d with
    | True_positive  -> { cr with true_positive  = cr.true_positive + 1}
    | False_negative -> { cr with false_negative = cr.false_negative + 1}
    | False_positive -> { cr with false_positive = cr.false_positive + 1}
    | True_negative  -> { cr with true_negative  = cr.true_negative + 1}

  (* From "A Simple Generalisation of the Area Under the ROC Curve for Multiple
     Class Classification Problems" by Hand and Till 2001. *)
  let to_auc data =
    let to_p d = if d.predicted then d.probability else 1.0 -. d.probability in
    let sorted = List.sort (fun d1 d2 -> compare (to_p d1) (to_p d2)) data in
    let ranked = List.mapi (fun idx d -> idx, d) sorted in
    let (sr, n0, n1) =
      List.fold_left ranked
        ~f:(fun (sr,n0,n1) (i, d) ->
          if d.actual
          then (sr + i, n0 + 1, n1)
          else (sr, n0, n1 + 1))
        ~init:(0,0,0)
    in
    let sr_f = float (sr + n0) in (* Since mapi ranks starting from 0 *)
    let n0_f = float n0 in
    let n1_f = float n1 in
    (sr_f -. n0_f *. (n0_f +. 1.0) *. 0.5) /. (n0_f *. n1_f)

  let to_descriptive data =
    let cr  = List.fold_left ~f:update_classification_record ~init:empty_cr data in
    let auc = to_auc data in
    let true_positive   = float cr.true_positive in
    let false_negative  = float cr.false_negative in
    let false_positive  = float cr.false_positive in
    let true_negative   = float cr.true_negative in
    let positive        = true_positive +. false_negative in
    let negative        = false_positive +. true_negative in
    { sensitivity         = true_positive /. positive
    ; specificity         = true_negative /. negative
    ; positive_predictive = true_positive /. (true_positive +. false_positive)
    ; negative_predictive = true_negative /. (false_negative +. true_negative)
    ; accuracy            = (true_positive +. true_negative) /. (negative +. positive)
    ; area_under_curve    = auc
    }

  let trapezoid_area (x1,y1) (x2,y2) =
    let xd = x2 -. x1 in
    let yp = y2 -. y1 in
    xd *. (y1 +. yp *. 0.5)

  (* (false_positive_rate, true_positive_rate) will add (0,0) and (1,1) *)
  let cross_validated_auc data =
    let bottom_left = 0.0, 0.0 in
    let top_right   = 1.0, 1.0 in
    let last, area =
      Array.fold_left (fun (prev_p, sum) point ->
        point, sum +. (trapezoid_area prev_p point))
        (bottom_left, 0.0) data
    in
    area +. (trapezoid_area last top_right)

end

let evaluate_performance = BinaryClassificationPerformance.to_descriptive

let cross_validated_auc = BinaryClassificationPerformance.cross_validated_auc
