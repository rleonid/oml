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
open Statistics.Distributions
module P = Probability

let () =
  let add_random_test
    ?title ?nb_runs ?nb_tries ?classifier
    ?reducer ?reduce_depth ?reduce_smaller ?random_src gen f spec =
    Test.add_random_test_group "Distributions"
      ?title ?nb_runs ?nb_tries ?classifier
      ?reducer ?reduce_depth ?reduce_smaller ?random_src gen f spec
  in
  let add_partial_random_test
    ?title ?nb_runs ?nb_tries ?classifier
    ?reducer ?reduce_depth ?reduce_smaller ?random_src gen f spec =
    Test.add_partial_random_test_group "Distributions"
      ?title ?nb_runs ?nb_tries ?classifier
      ?reducer ?reduce_depth ?reduce_smaller ?random_src gen f spec
  in
  let beta_cdf = beta_cdf ~alpha:1.0 ~beta:1.0 in

  (* These values easily cause erfc to underflow. *)
  let mean_o = Gen.(option bool (make_float (-1.) 1.0)) in
  let std_o  = Gen.(option bool (make_float (-1.) 1.0)) in
  add_random_test
    ~title:"Normal_cdf is between 0 and 1."
    Gen.(zip3 mean_o std_o (make_float (-1e1) (1e1)))
    (fun (mean,std,x) ->
      within (Closed 0.0, Closed 1.0) (normal_cdf ?mean ?std x :> float))
    Spec.([just_postcond_pred is_true]);

  add_random_test
    ~title:"Normal_quantile works."
    Gen.(zip3 mean_o std_o (make_float 0.0 1.0))
    (fun (mean,std,p) ->
      let _ = normal_quantile ?mean ?std (P.restrict p) in
      true (* TODO *))
    Spec.([just_postcond_pred is_true]);

  add_random_test
    ~title:"Beta cdf alpha=beta=1, assert f(x)=x"
    ~nb_runs:200
    Gen.(make_float 0. 1.)
    (fun weight ->
      let result = beta_cdf weight in
      not (significantly_different_from weight (result :> float)))
    Spec.([just_postcond_pred is_true]);

  let prob f = Spec.(zip3 f always always) in
  add_partial_random_test
    ~title:"Normal_cdf and normal_quantile are inverses."
    Gen.(zip3 (make_float (-0.5) 1.5) (make_float (-100.) 100.) (make_float (-100.) 100.))
    (fun (p, mean, std) ->
      let pp = normal_cdf ~mean ~std (normal_quantile ~mean ~std (P.restrict p)) in
      equal_floats ~d:1e-9 (pp :> float) p)
    Spec.([ prob (fun p -> p < 0.0 || p > 1.0)   ==> is_exception is_invalid_arg
          ; prob (fun p -> p >= 0.0 && p <= 1.0) ==> is_result is_true
          ]);

  let prob f = Spec.(zip2 f always) in
  add_partial_random_test
    ~title:"Student_cdf and student_quantile are inverses."
    Gen.(zip2 (make_float (-0.5) 1.5) (make_int 1 1000))
    (fun (p, degrees_of_freedom) ->
      let pp = student_cdf ~degrees_of_freedom 
          (student_quantile ~degrees_of_freedom (P.restrict p))
      in
      equal_floats ~d:1e-6 (pp :> float) p)
    Spec.([ prob (fun p -> p < 0.0 || p > 1.0)   ==> is_exception is_invalid_arg
          ; prob (fun p -> p >= 0.0 && p <= 1.0) ==> is_result is_true
          ]);

 let rec next_digit ~base previous =
    List.rev previous |> function
      | [] -> [1]
      | trailing_digit::rst ->
    begin
      if trailing_digit != (base - 1) then
       (trailing_digit+1)::rst |> List.rev
      else
        (next_digit ~base (List.rev rst))@[0]
    end in
  add_random_test
    ~title:"Benford pdf, assert digit sequences of same length sum to 1"
    ~nb_runs:200
    Gen.(zip2 (make_int 2 10)(make_int 1 5))
    (fun (base, num_digits) ->
      let rec first_seq l c =
        if c == num_digits then l else
          first_seq (0::l) (c+1)
        in
      let digits = List.rev @@ first_seq [1] 1 in
      let rec helper digits =
        if List.length digits > num_digits then 0.0 else
        (benford_pdf_seq ~base digits) +. helper (next_digit ~base digits) in
      let result = helper digits in
      not (significantly_different_from ~d:0.001 1.0 result))
    Spec.([just_postcond_pred is_true]);

  ()

