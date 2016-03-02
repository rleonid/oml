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
open Printf
open Util
open Uncategorized.Functions
module P = Probability

let fac i =
  let rec loop a i =
    if i = 0
    then a
    else loop (a * i) (i - 1)
  in loop 1 i

let () =
  let add_random_test
    ?title ?nb_runs ?nb_tries ?classifier
    ?reducer ?reduce_depth ?reduce_smaller ?random_src gen f spec =
    Test.add_random_test_group "Functions"
      ?title ?nb_runs ?nb_tries ?classifier
      ?reducer ?reduce_depth ?reduce_smaller ?random_src gen f spec
  in
  let add_partial_random_test
    ?title ?nb_runs ?nb_tries ?classifier
    ?reducer ?reduce_depth ?reduce_smaller ?random_src gen f spec =
    Test.add_partial_random_test_group "Functions"
      ?title ?nb_runs ?nb_tries ?classifier
      ?reducer ?reduce_depth ?reduce_smaller ?random_src gen f spec
  in
  (* Gamma *)
  add_random_test
    ~title:"Functions, gamma defined for (0+dx,1e2) positive values."
    Gen.(make_float Util.dx 1e2)
    gamma
    Spec.([ just_postcond_pred is_nonzero_float]);

  add_random_test
    ~title:"Gamma is equivalent to factorial for manageable ints."
    ~nb_runs:14
    Gen.(make_int 1 10)
    (fun i -> (float (fac (i - 1)), (gamma (float i))))
    Spec.([ just_postcond_pred (fun (x, y) -> equal_floats ~d:1e1 x y)]);

  (* Log gamma *)
  add_random_test
    ~title:"Log gamma defined for positive values."
    Gen.(bpos_float max_float)
    ln_gamma
    Spec.([ just_postcond_pred is_nonzero_float ]);

  add_random_test
    ~title:"(Log)-gamma is equivalent to factorial for manageable ints."
    ~nb_runs:14
    Gen.(make_int 1 10)
    (fun i -> (float (fac (i - 1)), exp (ln_gamma (float i))))
    Spec.([ just_postcond_pred (fun (x, y) -> equal_floats ~d:1e1 x y)]);

  add_random_test
    ~title:"(Log) gamma function statisfies multiplicative recurrence (aka Bohr-Mollerup 2)."
    ~nb_runs:1000
    Gen.(bpos_float max_float)
    (fun x -> log x +. ln_gamma x, ln_gamma (x +. 1.0))
    Spec.([just_postcond_pred (fun (x, y) -> equal_floats ~d:1. x y)]);

  (* Beta. *)
  add_random_test
    ~title:"Beta function is symmetric."
    Gen.(zip2 (make_float Util.dx 1e5) (make_float Util.dx 1e5))
    (fun (x, y) -> beta x y = beta y x)
    Spec.([just_postcond_pred is_true]);

  add_random_test
    ~title:"Adding one to x in beta function is like scaling."
    Gen.(zip2 (make_float Util.dx 1e5) (make_float Util.dx 1e5))
    (fun (x, y) -> beta x y = (beta (x +. 1.0) y) *. (x/. (x+.y)))
    Spec.([just_postcond_pred is_true]);

  (* Log beta *)
  add_random_test
    ~title:"Log beta function is symmetric."
    Gen.(zip2 (make_float Util.dx 1e5) (make_float Util.dx 1e5))
    (fun (x, y) -> ln_beta x y = ln_beta y x)
    Spec.([just_postcond_pred is_true]);

  (* A pretty poor test. *)
  add_random_test
    ~title:"log of multi beta with 2 args is same as beta"
    Gen.(zip2 (make_float Util.dx 1e5) (make_float Util.dx 1e5))
    (fun (x, y) -> ln_beta x y = ln_multivariate_beta [|x;y|])
    Spec.([just_postcond_pred is_true]);

  (* Chi *)
  add_random_test
    ~title:"Chi less and greater should sum to 1.0."
    Gen.(zip2 (make_int 2 100) (make_float Util.dx 1e3))
    (fun (df, x) ->
      let l = chi_square_less df x in
      let g = chi_square_greater df x in
      equal_floats ~d:1e-8 1.0 (l +.g))
    Spec.([just_postcond_pred is_true]);

  (* Softmax. *)
  let temp_opt =
    Gen.(select_array [| 0.0; 0.5; 1.0; 2.0; 1000.0 |] (sprintf "%0.4f")
         |> option bool)
  in
  let bad_spec (t_opt, arr) = t_opt = Some 0.0 || arr = [||] in
  add_partial_random_test
    ~title:"Softmax obeys bounds"
    Gen.(zip2 temp_opt (array (make_int 0 100) (bfloat max_float)))
    (fun (temperature, weights) ->
      let _ = P.softmax ?temperature weights in
      true)
    Spec.([ bad_spec     ==> is_exception is_invalid_arg
          ; not bad_spec ==> is_result is_true
          ]);

  ()
