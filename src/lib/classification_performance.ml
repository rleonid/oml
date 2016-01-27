
module List = ListLabels

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
    let sorted = List.sort ~cmp:(fun d1 d2 -> compare (to_p d1) (to_p d2)) data in
    let ranked = List.mapi ~f:(fun idx d -> idx, d) sorted in
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
