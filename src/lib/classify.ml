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

module type Classifier_intf = sig
  include Data_intf
  include Optional_arg_intf

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

module LrCommon(Data: Continuous_encoded_data_intf) = struct

  open Lacaml_D

  type feature = Data.feature
  type clas = Data.clas

  type samples = (clas * feature) list

  let safe_encoding =
    to_safe_encoding_size_checked "Continuous_encoded_data_intf"
      Data.size Data.encoding

  type spec = float
  let default = 1e-4

  let copy1 arr = Array.init (Data.size + 1) (function | 0 -> 1. | i -> arr.(i - 1))

  (* map classes to [1;2 ... 3], convert features to matrix and run Softmax *)
  let estimate ~method_name ~class_bound ~to_t ?(spec=default) ?(classes=[]) data =
    let class_bound =
      match class_bound with
      | None   -> fun n -> n
      | Some b -> min b
    in
    if data = [] then
      invalidArg "Classify.%s.estimate: Nothing to train on!" method_name
    else
      let error_on_new = classes <> [] in
      let assigned_cls_assoc =
        ref (List.mapi ~f:(fun i c -> c, class_bound (i + 1)) classes)
      in
      let classes =
        List.mapi data ~f:(fun idx (c, _) ->
          try
            List.assoc c !assigned_cls_assoc
          with Not_found ->
            if error_on_new then
              invalidArg "Found a new (unexpected) class at datum %d" idx
            else
              let n = class_bound (List.length !assigned_cls_assoc + 1) in
              assigned_cls_assoc := (c, n) :: !assigned_cls_assoc;
              n)
        |> Array.of_list
      in
      if List.length !assigned_cls_assoc < 2 then
        invalidArg "Trying to estimate Log Reg on less than 2 classes."
      else
        let ftrs =
          List.map (fun (_, f) -> copy1 (safe_encoding f)) data
          |> Array.of_list
          |> Mat.of_array
        in
        let weights = Softmax_regression.regress ~lambda:spec ftrs classes in
        let sortedc =
          List.sort ~cmp:(fun (_, n1) (_, n2) -> compare n1 n2)
            !assigned_cls_assoc
          |> List.map ~f:fst
        in
        to_t weights sortedc
 
end

module LogisticRegression(Data: Continuous_encoded_data_intf)
  : sig
    include Classifier_intf with type feature = Data.feature
                            and type clas = Data.clas
                            and type spec = float

    val coefficients : t -> float array

    val base_class : t -> clas

  end = struct

  include LrCommon(Data)
  open Lacaml_D

  type t =
    { weights    : vec
    ; classes    : clas list
    }

  let coefficients t = Vec.to_array t.weights

  let base_class t = List.hd t.classes

  let proba w x = Float.(1. / (1. + exp (dot w x)))

  let eval lr feature =
    let a = safe_encoding feature in
    let m = Vec.of_array (copy1 a) in
    let p = proba lr.weights m in
    (base_class lr, p) ::
      (List.map (fun c -> c, (1. -. p)) (List.tl lr.classes))

  let estimate = estimate
      ~method_name:"LogisticRegression"
      ~class_bound:(Some 2)
      ~to_t:(fun weights classes ->
              let r1 = Mat.copy_row weights 1 in
              let r2 = Mat.copy_row weights 2 in
              { weights = Vec.sub r2 r1; classes})

end

module MulticlassLogisticRegression(Data: Continuous_encoded_data_intf)
  : sig
    include Classifier_intf with type feature = Data.feature
                            and type clas = Data.clas
                            and type spec = float

    val coefficients : t -> float array array

    val class_order : t -> clas list

  end = struct

  include LrCommon(Data)
  open Lacaml_D

  type t =
    { weights : mat
    ; classes : clas list
    }

  let coefficients t = Mat.to_array t.weights

  let class_order t = t.classes

  let eval lr feature =
    let a   = safe_encoding feature in
    let x_i = Vec.of_array (copy1 a) in
    let prs = Softmax_regression.classify_v lr.weights x_i in
    List.map2 ~f:(fun c (_, p) -> (c, p)) lr.classes prs

  let estimate = estimate
      ~method_name:"MulticlassLogisticRegression"
      ~class_bound:None
      ~to_t:(fun weights classes -> { weights ; classes})

end

module Performance = struct
  include Classification_performance
end
