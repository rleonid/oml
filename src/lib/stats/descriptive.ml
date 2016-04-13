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

open Util
let invalid_arg ~f fmt = invalid_arg ~m:"Descriptive" ~f fmt

(* TODO: A tiny optimization; Once bench mark code has been written, it might
   be useful to compare the logic in this code where variables such as the
   length as a float are passed into methods specified for that. To avoid
   converting this value redundantly. Specifically:

   let mean_n m arr = (Array.sumf arr) /. m

   let mean arr = mean_n (float (Array.length arr)) arr
*)

let mean arr = (Array.sumf arr) /. float (Array.length arr)

let median arr =
  let sorted = Array.copy arr in
  Array.sort compare sorted;
  let n = Array.length arr in
  let m = n / 2 in
  if n mod 2 = 0
  then (sorted.(m - 1) +. sorted.(m)) /. 2.0
  else sorted.(m)

let var ?population_mean ?(biased=false) arr =
  let m, known_mean =
    match population_mean with
    | Some m -> m, true
    | None -> mean arr, false
  in
  let v = mean (Array.map (fun x -> (x -. m) *. (x -. m)) arr) in
  if known_mean || biased then v else
    let n = float (Array.length arr) in
    (n /. (n -.  1.0)) *. v

let sd ?population_mean ?(biased=false) arr =
  sqrt (var ?population_mean ~biased arr)

let ad ?population ?(center=`Median) arr =
  match center with
  | `Mean ->
    let m = match population with | None -> mean arr | Some m ->m in
    mean (Array.map (fun x -> abs_float (x -. m)) arr)
  | `Median ->
    let m = match population with | None -> median arr | Some m ->m in
    median (Array.map (fun x -> abs_float (x -. m)) arr)

let covariance ?population_means ?(biased=false) x y =
  let x_m, y_m, known_mean =
    match population_means with
    | Some (x_m, y_m) -> x_m,    y_m,    true
    | None            -> mean x, mean y, false
  in
  let v = mean (Array.map2 (fun x_i y_i -> (x_i -. x_m) *. (y_i -. y_m)) x y) in
  if known_mean || biased then v else
    let n = float (Array.length x) in
    (n /. (n -.  1.0)) *. v

let correlation x y =
  (* We specifiy a biased form for var/covariance to having consistent
     division by 'n'. *)
  let var = var ~biased:true in
  let cov = covariance ~biased:true in
  (cov x y) /. (sqrt ((var x) *. (var y)))

let autocorrelation lag ar =
  let m = Array.length ar - lag in
  correlation (Array.sub ar 0 m) (Array.sub ar lag m)

let moment n arr =
  let p = float n in
  let m = mean arr in
  mean (Array.map (fun x -> (x -. m) ** p) arr)

let skew ?(biased=false) arr =
  let std = sd ~biased:true arr in
  let s = (moment 3 arr) /. (std ** 3.0) in
  if biased then s else
    let n = float (Array.length arr) in
    Float.(s * (sqrt (n * (n - 1.0))) / (n-2.0))

let kurtosis ?(biased=false) arr =
  let k = (moment 4 arr) /. ((var ~biased:true arr) ** 2.0) -. 3.0 in
  if biased then k else
    let n = float (Array.length arr) in
    Float.(((n - 1.0) * ((n + 1.0) * k + 6.0)) / ((n - 2.0) * (n - 3.0)))

(* See "Standard errors: A review and evaluation of standard error estimators
  using Monte Carlo simulations" by Harding 2014, for a source of the standard
  error calcuations. *)
let var_standard_error arr =
  let n = float (Array.length arr) in
  let v = var arr in
  Float.(sqrt (2.0 / (n - 1.0)) * v)

let skew_standard_error arr =
  (* Older: sqrt ( 6.0 /. (float (Array.length arr))) *)
  let n = float (Array.length arr) in
  Float.(sqrt ((6.0 * n * (n - 1.0)) / ((n - 2.0) * (n + 1.0) * (n + 3.0))))

let kurtosis_standard_error arr =
  (* Older: sqrt ( 24.0 /. (float (Array.length arr))) *)
  let n = float (Array.length arr) in
  Float.(2.0 * (skew_standard_error arr) * sqrt ((n * n - 1.0) / ((n - 3.0) * (n - 5.0))))

let var_statistic arr =
  (var arr) /. (var_standard_error arr)

let skew_statistic arr =
  (skew arr) /. (skew_standard_error arr)

let kurtosis_statistic arr =
  (kurtosis arr) /. (kurtosis_standard_error arr)

type skew_classification =
  [ `Negative | `Slightly_negative | `Normal | `Slightly_positive | `Positive ]

let classify_skew arr =
  let s = skew_statistic arr in
  if s < -2.0 then `Negative
  else if s < -1.0 then `Slightly_negative
  else if s > 2.0 then `Positive
  else if s > 1.0 then `Slightly_positive
  else `Normal

type kurtosis_classification =
  [ `Skinny | `Slightly_skinny | `Fat | `Slightly_fat | `Normal ]

let classify_kurtosis arr =
  let s = kurtosis_statistic arr in
  if s < -2.0 then `Skinny
  else if s < -1.0 then `Slightly_skinny
  else if s > 2.0 then `Fat
  else if s > 1.0 then `Slightly_fat
  else `Normal

type summary =
  { size     : int
  ; min      : float
  ; max      : float
  ; mean     : float
  ; std      : float
  ; var      : float
  ; skew     : float * skew_classification
  ; kurtosis : float * kurtosis_classification
  }

let summary ?(biased=false) arr =
  let v = var ~biased arr in
  let s = skew ~biased arr in
  let k = kurtosis ~biased arr in
  let sc = classify_skew arr in
  let kc = classify_kurtosis arr in
  { size     = Array.length arr
  ; min      = Array.min arr
  ; max      = Array.max arr
  ; mean     = mean arr
  ; std      = sqrt v
  ; var      = v
  ; skew     = s, sc
  ; kurtosis = k, kc
  }

let specific_h buckets arr =
  if Array.length buckets = 0 then [||] else
    let tarr = Array.map (fun t -> (t,0)) buckets in
    (* Sort this array only one array constructor. *)
    Array.sort (fun (t1, _) (t2,_) -> compare t1 t2) tarr;
    Array.iter (fun e ->
        let idx = Array.binary_search (fun (t,_) -> compare e t) tarr in
        if idx > -1 then begin
          let t,c = Array.get tarr idx in
          Array.set tarr idx (t, succ c)
        end) arr;
    tarr

let custom_h ~mx ~mn ~width arr =
  let size = truncate (ceil ((floor ((mx -. mn) /. width)) +. 1.0)) in
  let harr = Array.init size (fun i -> (mn +. (float i) *. width, 0)) in
  Array.iter (fun v ->
      let idx = truncate ((v -. mn) /. width) in
      let width, count = harr.(idx) in
      harr.(idx) <- (width, count + 1)) arr;
    harr

let histogram width_setting arr =
  match width_setting with
  | `Specific buckets -> specific_h buckets arr
  | `Width width      -> let mn   = Array.min arr in
                         let mx   = Array.max arr in
                         custom_h ~mx ~mn ~width arr
  | `Buckets n        -> let mn   = Array.min arr in
                         let mx   = Array.max arr in
                         let width  = (mx -. mn) /. (float n) in
                         custom_h ~mx ~mn ~width arr

let geometric_mean arr =
  (* TODO: Determine a heuristic for when to do use the simpler method.*)
  exp (mean (Array.map log arr))

let harmonic_mean arr =
  1.0 /. (mean (Array.map (fun x -> 1.0 /. x) arr))

let spearman x y =
  let n = Array.length x in
  let ny = Array.length y in
  if ny <> n then
    invalid_arg ~f:"spearman" "array lengths don't match %d %d." n ny
  else
    let rx = Array.ranks ~average_ties:true x in
    let ry = Array.ranks ~average_ties:true y in
    correlation rx ry

let cosine x y =
  let p,a,b =
      Array.fold2 (fun (p,a,b) x y ->
        Float.(p + x * y, a + x * x, b + y * y))
          (0.,0.,0.) x y
  in
  p /. (sqrt a *. sqrt b)
