
open Util

let mean arr = (Array.sumf arr) /. float (Array.length arr)

let median arr =
  let sorted = Array.copy arr in
  Array.sort compare sorted;
  let n = Array.length arr in
  let m = n / 2 in
  if n mod 2 = 0
  then (sorted.(m - 1) +. sorted.(m)) /. 2.0
  else sorted.(m)

let var arr =
  let m = mean arr in
  mean (Array.map (fun x -> (x -. m) *. (x -. m)) arr)

let unbiased_var arr =
  let n = float (Array.length arr) in
  (n /. (n -.  1.0)) *. (var arr)

let covariance x y =
  let x_mean = mean x in
  let y_mean = mean y in
  mean (Array.map2 (fun x_i y_i -> (x_i -. x_mean) *. (y_i -. y_mean)) x y)

let correlation x y =
  (covariance x y) /. (sqrt ((var x) *. (var y)))

let auto_correlation lag ar =
  let m = Array.length ar - lag in
  correlation (Array.sub ar 0 m) (Array.sub ar lag m)

let moment n arr =
  let p = float n in
  let m = mean arr in
  mean (Array.map (fun x -> (x -. m) ** p) arr)

let skew arr =
  let std = sqrt (var arr) in
  (moment 3 arr) /. (std ** 3.0)

let kurtosis arr =
  (moment 4 arr) /. ((var arr) ** 2.0) -. 3.0

let unbiased_skew arr =
  let n = float (Array.length arr) in
  (skew arr) *. (sqrt (n *. (n -. 1.0))) /. (n -. 2.0)

let unbiased_kurtosis arr =
    let n = float (Array.length arr) in
    let k = (kurtosis arr) in
    ((n -. 1.0) *. ((n +. 1.0) *. k +. 6.0)) /. ((n -. 2.0) *. (n -. 3.0))

let var_standard_error arr =
  float (Array.length arr)

let skew_standard_error arr =
  sqrt ( 6.0 /. (float (Array.length arr)))

let kurtosis_standard_error arr =
  sqrt ( 24.0 /. (float (Array.length arr)))

let var_statistic arr =
  (unbiased_var arr) /. (var_standard_error arr)

let skew_statistic arr =
  (unbiased_skew arr) /. (skew_standard_error arr)

let kurtosis_statistic arr =
  (unbiased_kurtosis arr) /. (kurtosis_standard_error arr)

let classify_skew arr =
    let s = skew_statistic arr in
    if s < -2.0 then "negative skew"
    else if s < -1.0 then "slight negative skew"
    else if s > 2.0 then "positive skew"
    else if s > 1.0 then "slight positive skew"
    else "normal skew"

let classify_kurtosis arr =
    let s = kurtosis_statistic arr in
    if s < -2.0 then "skinny tails"
    else if s < -1.0 then "slight skinny tails"
    else if s > 2.0 then "fat tails"
    else if s > 1.0 then "slight fat tails"
    else "normal kurtosis"

let stat_classify arr =
  [ ("mean", mean arr)
  ; ("var", var arr)
  ; ("skew", skew arr)
  ; ("kurtosis", kurtosis arr)
  ]

let unbiased_classify arr =
  [ ("mean", mean arr)
  ; ("var", unbiased_var arr)
  ; ("skew", unbiased_skew arr)
  ; ("kurtosis", unbiased_kurtosis arr)
  ]

let unbiased_distribution_commentary arr =
  [ ("num obs", (float (Array.length arr)), "")
  ; ("min", Array.min arr, "")
  ; ("max", Array.max arr, "")
  ; ("mean", mean arr, "")
  ; ("std", (sqrt (unbiased_var arr)), "")
  ; ("sh", 16.0 *. (mean arr) /. (sqrt (unbiased_var arr)), "")
  ; ("skew", skew_statistic arr, (classify_skew arr))
  ; ("kurtosis", kurtosis_statistic arr, (classify_kurtosis arr))]

type dist_stats = { size     : int
                  ; mean     : float
                  ; var      : float
                  ; skew     : float
                  ; kurtosis : float
                  }

let dist_classify arr = { size = Array.length arr
                        ; mean = mean arr
                        ; var  = var arr
                        ; skew = skew arr
                        ; kurtosis = kurtosis arr
                        }

let unbiased_dist_classify arr =
  { size     = Array.length arr
  ; mean     = mean arr
  ; var      = unbiased_var arr
  ; skew     = unbiased_skew arr
  ; kurtosis = unbiased_kurtosis arr
  }

let histogram arr width_setting =
  let mn   = Array.min arr in
  let mx   = Array.max arr in
  let width =
    match width_setting with
    | `Width w   -> w
    | `Buckets n -> (mx -. mn) /. (float n)
  in
  let size = truncate (ceil ((floor ((mx -. mn) /. width)) +. 1.0)) in
  let harr = Array.init size (fun i -> (mn +. (float i) *. width, 0)) in
  Array.iter (fun v ->
      let idx = truncate ((v -. mn) /. width) in
      let width, count = harr.(idx) in
      harr.(idx) <- (width, count + 1)) arr;
    harr

let geometric_mean_definitional arr =
  let n = float (Array.length arr) in
  let p = Array.prodf arr in
  p ** (1.0 /. n)

let geometric_mean arr =
  (* TODO: Determine a heuristic for when to do use the simpler method.*)
  exp (mean (Array.map log arr))

let harmonic_mean arr =
  1.0 /. (mean (Array.map (fun x -> 1.0 /. x) arr))

let spearman x y =
  let n = Array.length x in
  let ny = Array.length y in
  if ny <> n then
    invalidArg "spearman: array lengths don't match %d %d." n ny
  else
    let rx = Array.ranks ~average_ties:true x in
    let ry = Array.ranks ~average_ties:true y in
    correlation rx ry
