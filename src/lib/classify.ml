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

type ('cls, 'ftr) naive_bayes =
  (* Store the class prior in last element of the array. *)
  { table             : ('cls * float array) list
  ; to_feature_array  : 'ftr -> int array
  ; features          : int
  }

let class_probabilities nb cls =
  let arr = List.assoc cls nb.table in
  arr.(nb.features), Array.sub arr 0 nb.features

let eval ?(bernoulli=false) nb b =
  let evidence = ref 0.0 in
  let to_likelihood class_probs =
    let idx = nb.to_feature_array b in
    if bernoulli then
      let set = Array.to_list idx in
      prod_arr (fun i ->
        if List.mem i ~set then
          class_probs.(i)
        else
          (1.0 -. class_probs.(i)))
        (Array.init nb.features (fun x -> x))
    else
      prod_arr (fun i -> class_probs.(i)) idx
  in
  let byc =
    List.map nb.table ~f:(fun (c, class_probs) ->
      let prior = class_probs.(nb.features) in
      let likelihood = to_likelihood class_probs in
      let prob  = prior *. likelihood in
      evidence := !evidence +. prob;
      (c, prob))
  in
  List.map byc ~f:(fun (c, prob) -> (c, prob /. !evidence))

let within a b x = max a (min x b)

let smoothing_to_prob = function
  | None    ->
      (fun count bkgrnd _ -> count /. bkgrnd)
  | Some sf ->
      let sf = within 0.0 1.0 sf in
      (fun count bkgrnd space_size ->
        (count +. sf) /. (bkgrnd +. sf *. space_size))

let estimate ?smoothing ?(classes=[]) ~feature_size to_ftr_arr data =
  if data = [] then
    invalidArg "Classify.estimate: Nothing to train on"
  else
    let aa = feature_size + 1 in
    let update arr idx =
      Array.iter (fun i -> arr.(i) <- arr.(i) + 1) idx;
      (* keep track of the class count at the end of array. *)
      arr.(feature_size) <- arr.(feature_size) + 1;
    in
    let error_on_new = classes <> [] in
    let init_class_lst = List.map classes ~f:(fun c -> (c, Array.make aa 0)) in
    let (total, all) =
      List.fold_left data
        ~f:(fun (total, asc) (label, feature) ->
          let n_asc =
            try
              let fr = List.assoc label asc in
              update fr (to_ftr_arr feature);
              asc
            with Not_found ->
              if error_on_new then
                invalidArg "Found a new (unexpected) class at datum %d" total
              else
                let fr = Array.make aa 0 in
                update fr (to_ftr_arr feature);
                (label, fr) :: asc
          in
          total + 1, n_asc)
        ~init:(0, init_class_lst)
    in
    let totalf = float total in
    let cls_sz = float (List.length all) in
    let to_prob = smoothing_to_prob smoothing in
    let table =
      List.map all ~f:(fun (cl, attr_count) ->
        let prior_count = float attr_count.(feature_size) in
        let likelihood =
          Array.init aa (fun i ->
            if i = feature_size then    (* Store the prior at the end. *)
              to_prob prior_count totalf cls_sz
            else
              to_prob (float attr_count.(i)) prior_count 2.0)   (* Binary. *)
        in
        cl, likelihood)
    in
    { table
    ; to_feature_array = to_ftr_arr
    ; features = feature_size
    }

type ('cls, 'ftr) naive_bayes_mv =
  { table             : ('cls * (float * float array array)) list
  ; to_feature_array  : 'ftr -> int array
  ; feature_sizes     : int array
  }

let class_probabilities_mv mvnb cls =
  let (prior, likelihood_arr) = List.assoc cls mvnb.table in
  (fun ftr -> 
    prior,
    Array.map2 (fun i lk_arr -> lk_arr.(i)) (mvnb.to_feature_array ftr) likelihood_arr)

let eval_mv mvnb feature =
  let evidence = ref 0.0 in
  let indices = mvnb.to_feature_array feature in
  let to_likelihood arr = prod_arr2 (fun i lk_arr -> lk_arr.(i)) indices arr in
  let byc =
    List.map mvnb.table ~f:(fun (c, (prior, class_probs)) ->
      let likelihood = to_likelihood class_probs in
      let prob  = prior *. likelihood in
      evidence := !evidence +. prob;
      (c, prob))
  in
  List.map byc ~f:(fun (c, prob) -> (c, prob /. !evidence))

let assoc_opt ~default f lst =
  try
    let g = List.assoc f lst in
    let r = List.remove_assoc f lst in
    g, r
  with Not_found ->
    default (), lst

let estimate_mv ?smoothing ?(classes=[]) ~feature_sizes to_ftr_arr data =
  if data = [] then
    invalidArg "Classify.estimate_mv: Nothing to train on"
  else
    let update arr feature =
      let ftr_arr = to_ftr_arr feature in
      Array.iteri (fun i j -> arr.(i).(j) <- arr.(i).(j) + 1) ftr_arr
    in
    let new_arr () = Array.map (fun i -> Array.make i 0) feature_sizes in
    let init_lst, default =
      match classes with
      | [] -> [], (fun () -> 0, new_arr ())
      | cl -> List.map (fun c -> c, (0, new_arr ())) cl,
              fun () -> invalidArg "Classify.estimate_mv classes have been specified."
    in
    let (total, all) =
      List.fold_left data
        ~f:(fun (total, asc) (label, feature) ->
          let (p, fr), n_asc = assoc_opt ~default label asc in
          update fr feature;
          total + 1, ((label, (p + 1, fr)) :: n_asc))
        ~init:(0, init_lst)
    in
    let to_prob = smoothing_to_prob smoothing in
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
    ; to_feature_array = to_ftr_arr
    ; feature_sizes
    }

type 'a gauss_bayes =
  { table     : ('a * float * (float * float) array) list
  ; features  : int
  }

let gauss_eval gb features =
  if Array.length features <> gb.features then
    invalidArg "Classify:gauss_eval: Expected a features array of %d features."
      gb.features;
  let prod =
    prod_arr2 (fun (mean,std) y -> Distributions.normal_pdf ~mean ~std y)
  in
  let evidence = ref 0.0 in
  let byc =
    List.map gb.table ~f:(fun (c, prior, class_params) ->
      let likelihood = prod class_params features in
      let prob       = prior *. likelihood in
      evidence := !evidence +. prob;
      (c, prob))
  in
  List.map byc ~f:(fun (c, prob) -> (c, prob /. !evidence))

let gauss_estimate ?(classes=[]) data =
  if data = [] then
    invalidArg "Classify.gauss_estimate: Nothing to train on!"
  else
    let update = Array.map2 Running.update in
    let init   = Array.map Running.init in
    let features = Array.length (snd (List.hd data)) in
    let init_cl  =
      let empty () = Array.make features Running.empty in
      List.map classes ~f:(fun c -> (c, (0, empty ())))
    in
    let error_on_new = classes <> [] in
    let total, by_class =
      List.fold_left data
        ~f:(fun (t, acc) (cls, attr) ->
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
          (c, class_prior, attr_params))
    in
    { table ; features }

module LD = Lacaml_D

type 'cls log_reg =
  { to_feature : float array -> LD.vec
  ; weights : LD.vec
  ; classes : ('cls * int) list
  }

let log_reg_eval lr features =
  let proba w x y = 1. /. (1. +. exp(-. float y *. LD.dot w x)) in
  let m = lr.to_feature features in
  List.map (fun (c,c_i) -> c, proba lr.weights m c_i) lr.classes

module Log_reg = struct

  (* Code modified from
     http://math.umons.ac.be/anum/fr/software/OCaml/Logistic_Regression/
  *)

  (* [logistic_grad_n_eval] returns the value of the function to maximize and store
    its gradient in [g]. *)
  let logistic_grad_n_eval ~lambda x y =
    let fy = Array.map float y in
    (fun w g ->
      let s = ref 0. in
      ignore(LD.copy ~y:g w);                   (* g ← w *)
      LD.scal (-. lambda) g;                    (* g = -λ w *)
      for i = 0 to Array.length x - 1 do
        let yi = fy.(i) in
        let e  = exp(-. yi *. LD.dot w x.(i)) in
        s := !s +. log1p e;
        LD.axpy x.(i) ~alpha:(yi *. e /. (1. +. e)) g;
      done;
      -. !s -. 0.5 *. lambda *. LD.dot w w)

  let log_reg ?(lambda=0.1) x y =
    let w = LD.Vec.make0 (LD.Vec.dim x.(0)) in
    ignore(Lbfgs.F.max (*~print:(Lbfgs.Every 10) *) (logistic_grad_n_eval ~lambda x y) w);
    w

end

let log_reg_estimate ~class_f data =
  if data = [] then
    invalidArg "Classify.log_reg_estimate: Nothing to train on!"
  else
    let classes = ref [] in
    let clss    =
      List.map (fun (c, _) ->
        (* TODO: are there better choices for these? *)
        let cc = if class_f c then 1 else -1 in
        if not (List.mem_assoc c !classes) then classes := (c, cc) :: !classes;
        cc) data
      |> Array.of_list
    in
    if List.length !classes < 2 then
      invalidArg "Trying to estimate Log Reg on only one classification."
    else
      let fs      = Array.length (snd (List.hd data)) in
                                        (* Fortran destination so 1 based. *)
      let to_f a  = LD.Vec.init (fs + 1) (function | 1 -> 1.0 | i -> a.(i - 2)) in
      let ftrs    = List.map (fun (_, f) -> to_f f) data |> Array.of_list in
      let weights = Log_reg.log_reg ftrs clss in
      { to_feature  = to_f
      ; weights
      ; classes = !classes
      }

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
