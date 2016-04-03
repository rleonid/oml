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

open Test_utils
open Util
open Statistics.Descriptive

let () =
  let add_simple_test = Test.add_simple_test_group "Descriptive" in
  let add_random_test
    ?title ?nb_runs ?nb_tries ?classifier
    ?reducer ?reduce_depth ?reduce_smaller ?random_src gen f spec =
    Test.add_random_test_group "Descriptive"
      ?title ?nb_runs ?nb_tries ?classifier
      ?reducer ?reduce_depth ?reduce_smaller ?random_src gen f spec
  in
  let sample_data = Array.range ~start:1. ~stop:6. () in
  add_simple_test ~title:"Mean for small data set."
    (fun () -> Assert.equalf (mean sample_data) 3.0);

  add_simple_test ~title:"Median for small data set."
    (fun () -> Assert.equalf (median sample_data) 3.0);

  let max_array_size = 1000 in
  let test_data b = Gen.(array (make_int 2 max_array_size) (bfloat b)) in

  (*add_random_test
    ~title:"Controllable mean"
    ~nb_runs:100
    (test_data 1e8)
    (fun data ->
      let n = Array.length data in
      let mu_s = data.(0) in
      let sumd = Array.sub data 1 (n - 1) |> Array.sumf in
      let x_j  = Float.(mu_s * (float n) - sumd) in
      data.(0) <- x_j;    (* now data has mean mu_s *)
      (*Printf.eprintf "mu %.16f mean data %.16f\n" mu_s (mean data); *)
      mu_s = mean data)
    Spec.([just_postcond_pred is_true]);*)

  add_random_test
    ~title:"Mean and median are bounded by the array."
    (test_data 1e8)
    (fun data ->
      let x = Array.max data in
      let n = Array.min data in
      let mn = mean data in
      let md = median data in
      n <= mn && mn <= x &&
      n <= md && md <= x)
    Spec.([just_postcond_pred is_true]);

  add_simple_test ~title:"Population var for small data set."
    (fun () -> Assert.equalf (var ~population_mean:3.0 sample_data) 2.0);

  add_simple_test ~title:"Var for small data set."
    (fun () -> Assert.equalf (var sample_data) 2.0);

  add_random_test
    ~title:"Population_var is just mean var."
    (test_data 1e8)
    (fun data ->
      let population_mean = mean data in
      (var ~population_mean data) = var data)
    Spec.([just_postcond_pred is_true]);

  add_simple_test ~title:"Unbiased_var for small data set."
    (fun () -> Assert.equalf (unbiased_var sample_data) 2.5);

  add_random_test
    ~title:"Unbiased_var is bigger than var."
    Gen.(array (make_int 2 max_array_size) (bfloat 1e8))
    (fun data -> unbiased_var data > var data)
    Spec.([just_postcond_pred is_true]);

  let two_arrays_same_size b =
    Gen.(matrix (lift 2 "2") (make_int 2 max_array_size) (bfloat b)
         |> map1 (fun m -> m.(0), m.(1))
          (fun (x,_) -> Printf.sprintf "Arrays of length %d" (Array.length x)))
  in
  add_random_test
    ~title:"Correlation and covariance are order invariant."
    (two_arrays_same_size 1e8)
    (fun (d1, d2) ->
      let co1 = covariance d1 d2 in
      let co2 = covariance d2 d1 in
      let cr1 = correlation d1 d2 in
      let cr2 = correlation d2 d1 in
      co1 = co2 && cr1 = cr2)
    Spec.([just_postcond_pred is_true]);

  add_random_test
    ~title:"Correlation and covariance have the same sign"
    (two_arrays_same_size 1e8)
    (fun (d1, d2) ->
      let co = covariance d1 d2 in
      let cr = correlation d1 d2 in
      let s v =  if v < 0.0 then -1 else if v > 0.0 then 1 else 0 in
      s co = s cr)
    Spec.([just_postcond_pred is_true]);

  add_random_test
    ~title:"Autocorrelation without lag is 1.0"
    (test_data 1e8)
    (fun data -> autocorrelation 0 data)
    Spec.([always ==> (fun x -> x = 1.0)]);

  (* TODO: this sometimes fails *)
  add_random_test
    ~title:"Moment 2 is just var."
    (test_data 1e8)
    (fun data -> moment 2 data = var data)
    Spec.([just_postcond_pred is_true]);

  add_simple_test ~title:"Skew for small data set."
    (fun () -> Assert.equalf (skew sample_data) 0.0);

  add_simple_test ~title:"Unbiased_skew for small data set."
    (fun () -> Assert.equalf (unbiased_skew sample_data) 0.0);

  add_simple_test ~title:"Kurtosis for small data set."
    (fun () -> Assert.equalf (kurtosis sample_data) (-1.3));

  let float_eq d = fun x y -> not (Util.significantly_different_from ~d x y) in
  add_simple_test ~title:"Unbiased_kurtosis for small data set."
    (fun () -> Assert.equalf ~eq:(float_eq 1e-15) (* rounding error :( *)
                (unbiased_kurtosis sample_data) (-1.2));

  add_random_test
    ~title:"Variance scales by square."
    (test_data 1e7)
    (fun data ->
      let cdata = Array.map (fun x -> x /. 10.0) data in
      let v1 = var data in
      let v2 = var cdata in
      float_eq 1e1 v1 (v2 *. 100.0))
    Spec.([just_postcond_pred is_true]);

  add_random_test
    ~title:"Variance statistic is scale invariant."
    (test_data 1e8)
    (fun data ->
      let cdata = Array.map (fun x -> x /. 10.0) data in
      let sv1 = var_statistic data in
      let sv2 = var_statistic cdata in
      float_eq 1e-13 sv1 sv2)
    Spec.([just_postcond_pred is_true]);

  (* This test is kind of bulky and might be better separated? *)
  add_random_test
    ~title:"Skew and kurtosis (and their statistics) are scale invariant."
    (test_data 1e8)
    (fun data ->
      let std   = sqrt (var data) in
      let sk    = skew data in
      let ku    = kurtosis data in
      let sks   = skew_statistic data in
      let kus   = kurtosis_statistic data in
      let sdata = Array.map (fun x -> x /. std) data in
      let sk_c  = skew sdata in
      let ku_c  = kurtosis sdata in
      let sks_c = skew_statistic sdata in
      let kus_c = kurtosis_statistic sdata in
      float_eq 1e-13 sk sk_c &&
      float_eq 1e-13 ku ku_c &&
      float_eq 1e-13 sks sks_c &&
      float_eq 1e-13 kus kus_c)
    Spec.([just_postcond_pred is_true]);

  (* TODO: definitely need something better than this*)
  add_random_test
    ~title:"We can compute standard errors and other stats."
    (test_data 1e8)
    (fun data ->
      let _ = var_standard_error data in
      let _ = skew_standard_error data in
      let _ = kurtosis_standard_error data in
      let _ = classify_skew data in
      let _ = classify_kurtosis data in
      true)
    Spec.([just_postcond_pred is_true]);

  (* Just a regression test to make sure we don't break the 1-1. *)
  add_random_test
    ~title:"Unbiased_summary is just that."
    (test_data 1e8)
    (fun data ->
      let s = unbiased_summary data in
      s.size      = Array.length data &&
      s.min       = Array.min data &&
      s.max       = Array.max data &&
      s.mean      = mean data &&
      s.std       = sqrt (unbiased_var data) &&
      s.var       = unbiased_var data &&
      s.skew      = (unbiased_skew data, classify_skew data) &&
      s.kurtosis  = (unbiased_kurtosis data, classify_kurtosis data))
    Spec.([just_postcond_pred is_true]);

  add_random_test
    ~title:"A histograms buckets have all the elements."
    Gen.(zip2 (bpos_float max_float) (test_data 1e8))
    (fun (w, data) ->
      let hist = histogram (`Width w) data in
      let nume = Array.map snd hist |> Array.fold_left (+) 0 in
      Array.length data = nume)
    Spec.([just_postcond_pred is_true]);

  add_random_test
    ~title:"A histograms buckets have all the elements, 2."
    Gen.(zip2 (make_int 1 (10 * max_array_size)) (test_data 1e8))
    (fun (b, data) ->
      let hist = histogram (`Buckets b) data in
      let nume = Array.map snd hist |> Array.fold_left (+) 0 in
      Array.length data = nume)
    Spec.([just_postcond_pred is_true]);

  add_random_test
    ~title:"One mean inequality to rule them all!"
    (test_data 1e8)
    (fun data ->
      let data = Array.map abs_float data in
      let m = mean data in
      let g = geometric_mean data in
      let h = harmonic_mean data in
      m >= g && g >= h)
    Spec.([just_postcond_pred is_true]);

  add_random_test
    ~title:"Spearman is bounded by -1 and 1"
    Gen.(zip2 (barray_float 1e8 max_array_size) (barray_float 1e8 max_array_size))
    (fun (arr1, arr2) ->
      let s = spearman arr1 arr2 in
      -1.0 <= s && s <= 1.0)
    Spec.([just_postcond_pred is_true]);

  add_random_test
    ~title:"Spearman is 1 for monotonic transformations"
    Gen.(barray_float 1e8 max_array_size)
    (fun arr1 ->
      let arr1 = Array.map abs_float arr1 in
      let arr2 = Array.map log arr1 in
      let s = spearman arr1 arr2 in
      s = 1.0)
    Spec.([just_postcond_pred is_true]);

  add_simple_test ~title:"Spearman exmaple"
    (fun () ->
      (* From wikipedia *)
      let data =
        [| (106, 7); (86,  0); (100, 27); (101, 50); (99,  28)
         ; (103, 29); (97,  20); (113, 12); (112, 6); (110, 17); |]
      in
      let x = Array.map (fun (x,_) -> float x) data in
      let y = Array.map (fun (_,y) -> float y) data in
      Assert.equal (spearman x y) (-29.0 /. 165.0));

  add_random_test
    ~title:"Cosine is size invariant!"
    Gen.(zip2 (barray_float 1e9 max_array_size)
                (barray_float 1e9 max_array_size))
    (fun (arr1,arr2) ->
       let cs1 = cosine arr1 arr2 in
       let nrm = let mx = Array.max arr1 in Array.map (fun x -> x /. mx) arr1 in
       let cs2 = cosine nrm arr2 in
       float_eq 1e-15 cs1 cs2)
    Spec.([just_postcond_pred is_true]);

  ()
