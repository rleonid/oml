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

module BinomialNaiveBayes(Data: Dummy_encoded_data_intf)
  : (Classifier_intf with type feature = Data.feature
                     and type clas = Data.clas)
  = struct

  type feature = Data.feature
  type clas = Data.clas

  type samples = (clas * feature) list

  type t =
    (* Store the class prior in last element of the array. *)
    { table       : (clas * float array) list
    ; e_bernoulli : bool
    }

  let eval nb b =
    let evidence = ref 0.0 in
    let to_likelihood class_probs =
      let idx = Data.encoding b in
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
    let byc =
      List.map nb.table ~f:(fun (c, class_probs) ->
        let prior = class_probs.(Data.size) in
        let likelihood = to_likelihood class_probs in
        let prob  = prior *. likelihood in
        evidence := !evidence +. prob;
        (c, prob))
    in
    List.map byc ~f:(fun (c, prob) -> (c, prob /. !evidence))

  type spec =
    { smoothing : float
    ; bernoulli : bool
    }

  let default = { smoothing = 0.0; bernoulli = false }

  let estimate ?(spec=default) ?(classes=[]) data =
    if data = [] then
      invalidArg "Classify.estimate: Nothing to train on"
    else
      let aa = Data.size + 1 in
      let update arr idx =
        Array.iter (fun i -> arr.(i) <- arr.(i) + 1) idx;
        (* keep track of the class count at the end of array. *)
        arr.(Data.size) <- arr.(Data.size) + 1;
      in
      let error_on_new = classes <> [] in
      let init_class_lst = List.map classes ~f:(fun c -> (c, Array.make aa 0)) in
      let (total, all) =
        List.fold_left data
          ~f:(fun (total, asc) (label, feature) ->
            let n_asc =
              try
                let fr = List.assoc label asc in
                update fr (Data.encoding feature);
                asc
              with Not_found ->
                if error_on_new then
                  invalidArg "Found a new (unexpected) class at datum %d" total
                else
                  let fr = Array.make aa 0 in
                  update fr (Data.encoding feature);
                  (label, fr) :: asc
            in
            total + 1, n_asc)
          ~init:(0, init_class_lst)
      in
      let totalf = float total in
      let cls_sz = float (List.length all) in
      let to_prob = smoothing_to_prob spec.smoothing in
      let table =
        List.map all ~f:(fun (cl, attr_count) ->
          let prior_count = float attr_count.(Data.size) in
          let likelihood =
            Array.init aa (fun i ->
              if i = Data.size then    (* Store the prior at the end. *)
                to_prob prior_count totalf cls_sz
              else
                to_prob (float attr_count.(i)) prior_count 2.0)   (* Binary. *)
          in
          cl, likelihood)
      in
      { table
      ; e_bernoulli = spec.bernoulli
      }

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

module CategoricalNaiveBayes(Data: Category_encoded_data_intf)
  : (Classifier_intf with type feature = Data.feature
                     and type clas = Data.clas)

  = struct

  type feature = Data.feature
  type clas = Data.clas

  type samples = (clas * feature) list

  type t =
    { table         : (clas * (float * float array array)) list
    }

  let class_probabilities t cls =
    let (prior, likelihood_arr) = List.assoc cls t.table in
    prior,
    (fun ftr ->
      Array.map2 (fun i lk_arr -> lk_arr.(i)) (Data.encoding ftr) likelihood_arr)

  let eval mvnb feature =
    let evidence = ref 0.0 in
    let indices = Data.encoding feature in
    let to_likelihood arr = prod_arr2 (fun i lk_arr -> lk_arr.(i)) indices arr in
    let byc =
      List.map mvnb.table ~f:(fun (c, (prior, class_probs)) ->
        let likelihood = to_likelihood class_probs in
        let prob  = prior *. likelihood in
        evidence := !evidence +. prob;
        (c, prob))
    in
    List.map byc ~f:(fun (c, prob) -> (c, prob /. !evidence))

  type spec = float
  let default = 0.0

  let estimate ?(spec=default) ?(classes=[]) data =
    if data = [] then
      invalidArg "Classify.estimate: Nothing to train on"
    else
      let update arr feature =
        let ftr_arr = Data.encoding feature in
        Array.iteri (fun i j -> arr.(i).(j) <- arr.(i).(j) + 1) ftr_arr
      in
      let new_arr () = Array.map (fun i -> Array.make i 0) Data.encoding_sizes in
      let init_lst, default =
        match classes with
        | [] -> [], (fun () -> 0, new_arr ())
        | cl -> List.map (fun c -> c, (0, new_arr ())) cl,
                fun () -> invalidArg "Classify.estimate classes have been specified."
      in
      let (total, all) =
        List.fold_left data
          ~f:(fun (total, asc) (label, feature) ->
            let (p, fr), n_asc = assoc_opt ~default label asc in
            update fr feature;
            total + 1, ((label, (p + 1, fr)) :: n_asc))
          ~init:(0, init_lst)
      in
      let to_prob = smoothing_to_prob spec in
      let totalf = float total in
      let numcls = float (List.length all) in
      let table =
        List.map all ~f:(fun (cl, (class_count, attr_count)) ->
          let prior      = to_prob (float class_count) totalf numcls in
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
      { table
      }

  end

module type Continuous_encoded_data_intf = sig
  include Data_intf
  val encoding : feature -> float array
  val size : int
end

module GaussianNaiveBayes(Data: Continuous_encoded_data_intf)
  : (Classifier_intf with type feature = Data.feature
                     and type clas = Data.clas)

  = struct

  type feature = Data.feature
  type clas = Data.clas

  type samples = (clas * feature) list

  type t =
    { table     : (clas * (float * (float * float) array)) list
    }

  let class_probabilities t cls =
    let (prior, dist_params) = List.assoc cls t.table in
    prior,
    (fun ftr ->
      Array.map2 (fun (mean,std) y -> Distributions.normal_pdf ~mean ~std y)
        dist_params (Data.encoding ftr))

  let eval gb feature =
    (*
    if Array.length features <> gb.features then
      invalidArg "Classify:eval: Expected a features array of %d features."
        gb.features; *)
    let prod =
      prod_arr2 (fun (mean,std) y -> Distributions.normal_pdf ~mean ~std y)
    in
    let evidence = ref 0.0 in
    let byc =
      List.map gb.table ~f:(fun (c, (prior, class_params)) ->
        let likelihood = prod class_params (Data.encoding feature) in
        let prob       = prior *. likelihood in
        evidence := !evidence +. prob;
        (c, prob))
    in
    List.map byc ~f:(fun (c, prob) -> (c, prob /. !evidence))

  type spec = unit
  let default = ()

  let estimate ?(spec=default) ?(classes=[]) data =
    if data = [] then
      invalidArg "Classify.estimate: Nothing to train on!"
    else
      let update = Array.map2 Running.update in
      let init   = Array.map Running.init in
      (*let features = Array.length (snd (List.hd data)) in*)
      let init_cl  =
        let empty () = Array.make Data.size Running.empty in
        List.map classes ~f:(fun c -> (c, (0, empty ())))
      in
      let error_on_new = classes <> [] in
      let total, by_class =
        List.fold_left data
          ~f:(fun (t, acc) (cls, ftr) ->
            let attr = Data.encoding ftr in
            try
              let (cf, rsar) = List.assoc cls acc in
              let acc'       = List.remove_assoc cls acc in
              let nrs        = update rsar attr in
              let cf'        = cf + 1 in
              (t + 1, (cls, (cf', nrs)) :: acc')
            with Not_found ->
              if error_on_new then
                invalidArg "Found a new (unexpected) class at datum %d" t
              else
                (t + 1, (cls, (1, (init attr))) :: acc))
          ~init:(0, init_cl)
      in
      let totalf = float total in
        (* A lot of the literature in estimating Naive Bayes focuses on estimating
          the parameters using Maximum Likelihood. The Running estimate of variance
          computes the unbiased form. Not certain if we should implement the
          n/(n-1) conversion below. *)
      let table =
        let select rs = rs.Running.mean, (sqrt rs.Running.var) in
        by_class
        |> List.map ~f:(fun (c, (cf, rsarr)) ->
            let class_prior = (float cf) /. totalf in
            let attr_params = Array.map select rsarr in
            (c, (class_prior, attr_params)))
      in
      { table }
  end

module LogisticRegression(Data: Continuous_encoded_data_intf)
  : (Classifier_intf with type feature = Data.feature
                     and type clas = Data.clas)

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

  let eval lr feature =
    let proba w x y = Float.(1. / (1. + exp(-. y * dot w x))) in
    let m = Vec.of_array (Data.encoding feature) in
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
        let ftrs    = List.map (fun (_, f) -> to_f (Data.encoding f)) data
                      |> Array.of_list
        in
        let weights = Log_reg.log_reg ftrs classes in
        { weights
        ; classes = !assigned_cls_assoc
        }

  let class_probabilities lr cls = failwith "Not implemented"

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
