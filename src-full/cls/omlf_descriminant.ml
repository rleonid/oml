

open Lacaml.D
open Oml_util
module LU = Omlf_lacaml_util
module Interfaces = Oml_classification_interfaces

module LDA(Data : Interfaces.Continuous_encoded_data) = struct
  type feature = Data.feature
  type class_ = Data.class_

  type samples = (class_ * feature) list
  type t =
    { cov_i : Mat.t
    ; prior_and_means : (class_ * (float * Vec.t)) list
    }

  let safe_encoding f =
    let e = Data.encoding f in
    let l = Array.length e in
    if l = Data.size then
      e
    else
      invalid_arg ~m:"Lda" ~f:"encoding" "size %d actual %d" Data.size l

  let likelihood ~x ~mean cov_i =
    let d = Vec.sub x mean in
    let p = -0.5 *. dot d (gemv cov_i d) in
    exp p

  type feature_probability = float

  let class_probabilities t cls =
    let prior, mean = List.assoc cls t.prior_and_means in
    prior,
    (fun ftr ->
       let x = Vec.of_array (safe_encoding ftr) in
       likelihood ~x ~mean t.cov_i)

  let eval t ftr =
    let evidence = ref 0.0 in
    let x = Vec.of_array (safe_encoding ftr) in
    (* Reuse Naive_bayes.eval_naive_bayes? *)
    t.prior_and_means
    |> List.map ~f:(fun (cls, (prior, mean)) ->
        let likelihood = likelihood ~x ~mean t.cov_i in
        let prob = prior *. likelihood in
        evidence := !evidence +. prob;
        cls, prob)
    |> List.map ~f:(fun (c,p) -> (c, p /. !evidence))

  type opt = float
  let opt ?(shrinkage=0.0) () = shrinkage
  let default = opt ()

  let shrink shrinkage a =
    let d = Mat.of_diag (Vec.make (Mat.dim1 a) shrinkage) in
    Mat.axpy ~alpha:(1.-.shrinkage) a d;
    d

  let apply_shrink opt cov =
    if opt < 0.0 || opt >= 1.0 then
      invalid_arg ~m:"Lda" ~f:"estimate" "invalid shrinkage value %f" opt
    else if opt = 0.0 then
      cov
    else
      shrink opt cov

  let estimate ?(opt=default) ?(classes=[]) samples =
    if samples = [] then
      invalid_arg ~m:"Lda" ~f:"estimate" "empty training data"
    else
      let module Cs = Set.Make (struct type t = class_ let compare = compare end) in
      let clsls = List.map ~f:fst samples in
      let masks =
        if classes = [] then
          LU.class_masks clsls
        else
          let css   = Cs.of_list classes in
          let cls_s = Cs.of_list clsls in
          if Cs.diff css cls_s <> Cs.empty then
            invalid_arg ~m:"Lda" ~f:"estimate" "Incorrect class specification"
          else
            LU.class_masks ~class_order:classes clsls
      in
      let mat   = samples
                  |> List.map ~f:(fun (_,f) -> safe_encoding f)
                  |> Array.of_list
                  |> Mat.of_array in
      let n     = float (Mat.dim1 mat) in
      let means = LU.class_means mat masks in
      let prior_and_means =
        List.mapi masks ~f:(fun i (cls, msk) ->
          let prior = msk.LU.size /. n in
          let meanv = Mat.col means (i + 1) in (* 1 based *)
          (cls, (prior, meanv)))
      in
      let cov = apply_shrink opt (LU.sample_covariance mat) in
      let _det, cov_i = LU.determinant_symmetric_and_inverse ~copy:false cov in
      { cov_i ; prior_and_means }

end (* LDA *)

module QDA(Data : Interfaces.Continuous_encoded_data) = struct
  type feature = Data.feature
  type class_ = Data.class_

  type samples = (class_ * feature) list
  type t = (class_ * (float * Vec.t * Mat.t * float)) list

  let safe_encoding f =
    let e = Data.encoding f in
    let l = Array.length e in
    if l = Data.size then
      e
    else
      invalid_arg ~m:"Qda" ~f:"encoding" "size %d actual %d" Data.size l

  let likelihood ~x ~mean cov_i norm =
    let d = Vec.sub x mean in
    let p = -0.5 *. dot d (gemv cov_i d) in
    exp p /. norm

  type feature_probability = float

  let class_probabilities t cls =
    let prior, mean, cov_i, norm = List.assoc cls t in
    prior,
    (fun ftr ->
       let x = Vec.of_array (safe_encoding ftr) in
       likelihood ~x ~mean cov_i norm)

  let eval t ftr =
    let evidence = ref 0.0 in
    let x = Vec.of_array (safe_encoding ftr) in
    (* Reuse Naive_bayes.eval_naive_bayes? *)
    t
    |> List.map ~f:(fun (cls, (prior, mean, cov_i, norm)) ->
        let likelihood = likelihood ~x ~mean cov_i norm in
        let prob = prior *. likelihood in
        evidence := !evidence +. prob;
        cls, prob)
    |> List.map ~f:(fun (c,p) -> (c, p /. !evidence))

  type opt = (bool * float)
  let opt ?(normalize=true) ?(shrinkage=0.0) () = (normalize, shrinkage)
  let default = opt ()

  let shrink shrinkage a =
    let d = Mat.of_diag (Vec.make (Mat.dim1 a) shrinkage) in
    Mat.axpy ~alpha:(1.-.shrinkage) a d;
    d

  let apply_shrink opt cov =
    if opt < 0.0 || opt >= 1.0 then
      invalid_arg ~m:"Qda" ~f:"estimate" "invalid shrinkage value %f" opt
    else if opt = 0.0 then
      cov
    else
      shrink opt cov

  let estimate ?(opt=default) ?(classes=[]) samples =
    if samples = [] then
      invalid_arg ~m:"Qda" ~f:"estimate" "empty training data"
    else
      let module Cs = Set.Make (struct type t = class_ let compare = compare end) in
      let clsls = List.map ~f:fst samples in
      let masks =
        if classes = [] then
          LU.class_masks clsls
        else
          let css   = Cs.of_list classes in
          let cls_s = Cs.of_list clsls in
          if Cs.diff css cls_s <> Cs.empty then
            invalid_arg ~m:"Qda" ~f:"estimate" "Incorrect class specification"
          else
            LU.class_masks ~class_order:classes clsls
      in
      let mat   = samples
                  |> List.map ~f:(fun (_,f) -> safe_encoding f)
                  |> Array.of_list
                  |> Mat.of_array in
      let n     = float (Mat.dim1 mat) in
      let means = LU.class_means mat masks in
      let prior_and_means =
        List.mapi masks ~f:(fun i (cls, msk) ->
          let prior = msk.LU.size /. n in
          let meanv = Mat.col means (i + 1) in (* 1 based *)
          (cls, (prior, meanv)))
      in
      let k     = float (Mat.dim2 mat) in
      let covs = LU.class_sample_covariance mat masks in
      List.map2 prior_and_means covs ~f:(fun (cls, (pr, mn)) (cls2, cov) ->
          assert (cls = cls2);
          let normalize, shrink_c = opt in
          let cov' = apply_shrink shrink_c cov in
          let det, cov_i = LU.determinant_symmetric_and_inverse ~copy:false cov' in
          let norm =
            if normalize then
              let d = exp (k *. (log 2. +. log pi) +. log det) in
              1. /. d
            else
              1.
          in
          (cls, (pr, mn, cov_i, norm)))

end (* QDA *)
