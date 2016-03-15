

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
    ; norm  : float
    ; means : (clas * (float * Vec.t)) list
    }

  let safe_encoding f =
    let e = Data.encoding f in
    let l = Array.length e in
    if l = Data.size then
      e
    else
      invalid_arg ~m:"Lda" ~f:"encoding" "size %d actual %d" Data.size l

  let likelihood ~x ~mean cov_i norm =
    let d = Vec.sub x mean in
    let p = -0.5 *. dot d (gemv cov_i d) in
    exp p /. norm

  type feature_probability = float

  let class_probabilities t cls =
    let prior, mean = List.assoc cls t.means in
    prior,
    (fun ftr ->
       let x = Vec.of_array (safe_encoding ftr) in
       likelihood ~x ~mean t.cov_i t.norm)

  (* This is a 'naive' (computes too much to just performn classification)
     approach, but for the specification of producing class probabilities. *)
  let eval t ftr =
    let evidence = ref 0.0 in
    let x = Vec.of_array (safe_encoding ftr) in
    (* Reuse Naive_bayes.eval_naive_bayes? *)
    t.means
    |> List.map ~f:(fun (cls, (prior, mean)) ->
        let likelihood = likelihood ~x ~mean t.cov_i t.norm in
        let prob = prior *. likelihood in
        evidence := !evidence +. prob;
        cls, prob)
    |> List.map ~f:(fun (c,p) -> (c, p /. !evidence))

  type opt = unit
  let default = ()

  let estimate ?(opt=default) ?(classes=[]) samples =
    ignore opt;
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
      let k     = float (Mat.dim2 mat) in
      let means = LU.class_means mat masks in
      let pri   =
        List.map2 ~f:(fun c m -> (c, m)) masks.LU.order (Mat.to_col_vecs_list means)
        |> List.map2 ~f:(fun s (c, m) -> (c, (s /. n, m))) masks.LU.sizes
      in
      let cov = LU.sample_covariance mat in
      let det, inv = LU.determinant_and_inverse ~copy:false cov in
      (* TODO: should the normalization be lazy? we don't need to normalize by
         this to classify. *)
      { cov_i = inv
      ; means = pri
      ; norm  = (exp ((k /. 2.) *. log (2.0 *. pi))) *. sqrt det
      }

end

