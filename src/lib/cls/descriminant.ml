

open Lacaml.D
open Util
module LU = Uncategorized.Lacaml_util
module List = ListLabels

module LDA(Data : Intf.Continuous_encoded_data) = struct
  type feature = Data.feature
  type clas = Data.clas

  type samples = (clas * feature) list
  type t =
    { cov_i : Mat.t
    ; means : (clas * (float * Vec.t)) list
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
    let prior, mean = List.assoc cls t.means in
    prior,
    (fun ftr ->
       let x = Vec.of_array (safe_encoding ftr) in
       likelihood ~x ~mean t.cov_i)

  let eval t ftr =
    let evidence = ref 0.0 in
    let x = Vec.of_array (safe_encoding ftr) in
    (* Reuse Naive_bayes.eval_naive_bayes? *)
    t.means
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

  let estimate ?(opt=default) ?(classes=[]) samples =
    if samples = [] then
      invalid_arg ~m:"Lda" ~f:"estimate" "empty training data"
    else
      let module Cs = Set.Make (struct type t = clas let compare = compare end) in
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
      (*let k     = float (Mat.dim2 mat) in *)
      let means = LU.class_means mat masks in
      let pri   =
        List.map2 ~f:(fun c m -> (c, m)) masks.LU.order (Mat.to_col_vecs_list means)
        |> List.map2 ~f:(fun s (c, m) -> (c, (s /. n, m))) masks.LU.sizes
      in
      let cov = LU.sample_covariance mat in
      let cov =
        if opt < 0.0 || opt >= 1.0 then
          invalid_arg ~m:"Lda" ~f:"estimate" "invalid shrinkage value %f" opt
        else if opt = 0.0 then
          cov
        else
          shrink opt cov
      in
      let _det, inv = LU.determinant_symmetric_and_inverse ~copy:false cov in
      { cov_i = inv
      ; means = pri
      }

end (* LDA *)
