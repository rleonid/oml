
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

type smoothing =
  { factor              : float
  ; feature_space_size  : int array
  }

let estimate ?smoothing ~feature_size to_ftr_arr data =
  if data = [] then
    invalidArg "Classify.estimate: Nothing to train on"
  else
    let aa = feature_size + 1 in
    let update arr idx =
      Array.iter (fun i -> arr.(i) <- arr.(i) + 1) idx;
      (* keep track of the class count at the end of array. *)
      arr.(feature_size) <- arr.(feature_size) + 1;
    in
    let (total, all) =
      List.fold_left data
        ~f:(fun (total, asc) (label, feature) ->
          let n_asc =
            try
              let fr = List.assoc label asc in
              update fr (to_ftr_arr feature);
              asc
            with Not_found ->
              let fr = Array.make aa 0 in
              update fr (to_ftr_arr feature);
              (label, fr) :: asc
          in
          total + 1, n_asc)
        ~init:(0, [])
    in
    let totalf = float total in
    let cls_sz = float (List.length all) in
    let to_prior_prob, to_lkhd_prob =
      match smoothing with
      | None ->
          (fun count bkgrnd _ -> count /. bkgrnd),
          (fun count bkgrnd _ -> count /. bkgrnd)
      | Some s ->
          (* TODO: Issue warning? Fail? *)
          let sf  = within 0.0 1.0 s.factor in
          let fss = Array.map float s.feature_space_size in
          (fun count bkgrnd space_size ->
              (count +. sf) /. (bkgrnd +. sf *. space_size)),
          (fun count bkgrnd idx ->
              (count +. sf) /. (bkgrnd +. sf *. fss.(idx)))
    in
    let table =
      List.map all ~f:(fun (cl, attr_count) ->
        let prior_count = float attr_count.(feature_size) in
        let likelihood =
          Array.init aa (fun i ->
            to_lkhd_prob (float attr_count.(i)) prior_count i)
        in
        (* Store the prior at the end. *)
        likelihood.(feature_size) <- to_prior_prob prior_count totalf cls_sz;
        cl, likelihood)
    in
    { table
    ; to_feature_array = to_ftr_arr
    ; features = feature_size
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

let gauss_estimate data =
  if data = [] then
    invalidArg "Classify.gauss_estimate: Nothing to train on!"
  else
    let update = Array.map2 Running.update in
    let init   = Array.map Running.init in
    let features = Array.length (snd (List.hd data)) in
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
            (t + 1, (cls, (1, (init attr))) :: acc))
        ~init:(0, [])
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
