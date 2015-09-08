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

(* Common functions for writing tests and specifications. *)

open Printf

(* Generators *)
module type FloatParameters = sig
  val largest_float : float
end

module FGen (Fp : FloatParameters) = struct
  include Kaputt.Abbreviations.Gen
  let nlarge  = -1.0 *. Fp.largest_float
  let float   = (fun r ->
      let _ = failwith "Do not use unbounded float generator!" in
      let s = Random.State.bool r in
      let x = Random.State.float r Fp.largest_float in
      if s then x else -.x)
      , string_of_float

  (* Bounded *)
  let bfloat m = (fun r ->
    let s = Random.State.bool r in
    let x = Random.State.float r m in
    if s then x else -.x),
    string_of_float

  let bpos_float m = filter ((<=) 0.0) (bfloat m)
  let bneg_float m = filter ((>=) 0.0) (bfloat m)
  let bnon_zero_float m = filter ((<>) 0.0) (bfloat m)

  (* Fixed length *)
  let fixed_length_array n  =
    let msg = "array length " ^ (string_of_int n) in
    array (lift n msg)

  let fixed_length_matrix r c e =
    let row_msg = "row length " ^ string_of_int r
    and col_msg = "col length " ^ string_of_int c in
    array (lift r row_msg) (array (lift c col_msg) e)

  let matrix (r, _) (c, _) (e, es) =
    (fun random ->
      let rows = r random and columns = c random in
      Array.init rows (fun _ ->
        Array.init columns (fun _ ->
          e random))),
    (fun m ->
      m
      |> Array.map (Kaputt.Utils.make_string_of_array es)
      |> Array.to_list
      |> String.concat "\n")

  let barray_float b n = fixed_length_array n (bfloat b)
  let bmatrix_float b r c = fixed_length_matrix r c (bfloat b)

  let print_float_array m =
    m
    |> Array.map (Kaputt.Utils.make_string_of_array string_of_float)
    |> Array.to_list
    |> String.concat "\n"

  let general_model_array b ~max_predictors ~max_samples =
    zip2 (make_int 2 max_predictors)
         (make_int 2 max_samples)
    (* +1 is for how we build samples models below. *)
    |> transform (fun (p, s) -> 1 + max p s, min p s)
    |> fun (rcg, _) ->
        (* TODO: refactor this into a bind. *)
        (fun random ->
          let (rows, columns) = rcg random in
            Array.init rows (fun _ ->
              Array.init columns (fun _ ->
                (fst (bfloat b)) random))),
        print_float_array

  let general_model b ~max_predictors ~max_samples =
    general_model_array b max_predictors max_samples
    |> map1 (fun m ->
      let data = Array.sub m 1 (Array.length m - 1) in
      let coef = m.(0) in
      let prod_column_vector m v =
        let row_l = Array.length m in
        let col_l = Array.length m.(0) in
        let n = col_l - 1 in
        let s = Pervasives.ref 0.0 in
        Array.init row_l (fun r ->
          s := 0.0;
          for i = 0 to n do
            s := !s +. v.(i) *. m.(r).(i)
          done;
          !s)
      in
      let resp = prod_column_vector data coef in
      (data, coef, resp))
      (fun (pred, coef, resp) ->
        sprintf "predictors: %s\n coefficients: %s\n response: %s\n"
          (print_float_array pred)
          (Kaputt.Utils.make_string_of_array string_of_float coef)
          (Kaputt.Utils.make_string_of_array string_of_float resp))

end

module Gen = FGen (struct let largest_float = max_float end)

(* Specifications *)
module Spec = struct

  let is_true x = x
  let is_false x = not x
  let just_postcond_pred p = Kaputt.Abbreviations.Spec.( always ==> p)
  let is_invalid_arg = function | Invalid_argument _ -> true | _ -> false
  include Kaputt.Abbreviations.Spec
end

module Assert = struct
  include Kaputt.Abbreviations.Assert
  let equalf = equal ~prn:(sprintf "%0.4f")
end

module TestMap = Map.Make(
  struct
    type t = string
    let compare = compare
  end)

module Test = struct
  include Kaputt.Abbreviations.Test

  let test_holder = ref TestMap.empty

  let add_simple_test_group group ?title f =
    let t = make_simple_test ?title f in
    let tests = try TestMap.find group !test_holder with Not_found -> [] in
    test_holder := TestMap.add group (t :: tests) !test_holder

  let add_random_test_group group ?title ?nb_runs ?nb_tries ?classifier
       ?reducer ?reduce_depth ?reduce_smaller ?random_src gen f spec =
    let t = make_random_test ?title ?nb_runs ?nb_tries ?classifier
       ?reducer ?reduce_depth ?reduce_smaller ?random_src gen f spec
    in
    let tests = try TestMap.find group !test_holder with Not_found -> [] in
    test_holder := TestMap.add group (t :: tests) !test_holder

  let add_partial_random_test_group group ?title ?nb_runs ?nb_tries ?classifier
       ?reducer ?reduce_depth ?reduce_smaller ?random_src gen f spec =
    let t = make_partial_random_test ?title ?nb_runs ?nb_tries ?classifier
       ?reducer ?reduce_depth ?reduce_smaller ?random_src gen f spec
    in
    let tests = try TestMap.find group !test_holder with Not_found -> [] in
    test_holder := TestMap.add group (t :: tests) !test_holder

  let launch_tests () =
    TestMap.bindings !test_holder
    |> List.sort compare
    |> List.iter (fun (group, tests) ->
        Printf.printf "--%s--\n%!" group;
        List.rev tests |> run_tests)

  let launch_test group =
    try run_tests (TestMap.find group !test_holder)
    with Not_found -> ()

end

type test_const =
  { float_range : int
  ; accuracy    : int
  ; nb_runs     : int
  ; pass_rate   : float
  }

let tc float_range accuracy nb_runs pass_rate =
  { float_range; accuracy; nb_runs; pass_rate }

let enot i = 10. ** (float i)

type constrained_test = test_const -> Test.t

let constrained_map = ref TestMap.empty

let add_constrained name (t : constrained_test) =
  constrained_map := TestMap.add name t !constrained_map

let f t =
  let cache = ref (Hashtbl.create 20) in
  (fun tc ->
    try Hashtbl.find !cache tc  
    with Not_found ->
      match Test.exec_test (t tc) with
      | Test.Report (passed, _total, _ , _, _) ->
          let y = (float passed) /. (float tc.nb_runs) -. tc.pass_rate in
          Hashtbl.add !cache tc y;
          y
      | Test.Passed
      | Test.Failed _
      | Test.Uncaught _
      | Test.Exit_code _ -> raise (Invalid_argument "What am I testing?"))

let iteration_limit = ref 10
let debug_ref = ref false 

let bisection_int ~epsilon ~lower ~upper f =
  let eq_zero v = abs_float v < epsilon in
  let root_inside l u = (l < 0.0 && u > 0.0) || (l > 0.0 && u < 0.0) in
  let midpoint lb ub = (lb + ub) / 2 in
  let rec loop lb ub =
    let mp = midpoint lb ub in
    if mp = lb || mp = ub then
      `CloseEnough mp
    else
      let flb = f lb in
      let fub = f ub in
      if !debug_ref then
        Printf.printf "lb %d ub %d flb %.16f fub %.16f \n%!" lb ub flb fub;
      (* Are we lucky? *)
      if eq_zero flb then
        `EqZero lb
      else if eq_zero fub then
        `EqZero ub
      (* if the user does not provide an interval
          where the function has an intersection. *)
      else if flb < 0.0 && fub < 0.0 then
        `Outside ub
      else if flb > 0.0 && fub > 0.0 then
        `Outside lb
      else
        let fmp = f mp in
        if eq_zero fmp then
          `EqZero mp
        else if root_inside flb fmp then (* go left! *)
          loop lb mp
        else if root_inside fmp fub then (* go right! *)
          loop mp ub
        else
          `IntermediateValueTheoremViolated mp
  in
  loop lower upper

let bisection_i ~epsilon ~lower ~upper f =
  match bisection_int ~epsilon ~lower ~upper f with
  | `CloseEnough v | `EqZero v -> v
  | `Outside o ->
      raise (Invalid_argument "outside")
  | `IntermediateValueTheoremViolated _ ->
      failwith "ivtv"

let range ?(incr=1) ~start ~stop () =
    if stop < start
    then [||]
    else
      Array.init (stop - start) (fun i -> i + start)

let solvei1 f ~start ~stop =
  let rec loop iter stop =
    if iter > !iteration_limit then
      let () = 
        if !debug_ref then
          Printf.eprintf "passed iteration limit %d stop: %d" !iteration_limit stop
        in
      0
    else
      try 
        let r = bisection_i ~epsilon:1e-9 ~lower:start ~upper:stop f in
        let () = if !debug_ref then Printf.eprintf "solved %d %d %d\n%!" start stop r in
        r
      with Invalid_argument _ ->
        loop (iter + 1) (stop * 2)
  in
  loop 0 stop

let solvei2 f ~start ~stop =
  let start1,start2 = start in
  let stop1,stop2 = stop in
  range ~start:start1 ~stop:stop1 () 
  |> Array.map (fun x -> x, solvei1 ~start:start2 ~stop:stop2 (f x))
 (* 
let solve3 f ~start ~stop =
  let start1,start2,start3 = start in
  let stop1,stop2,stop3 = stop in
  range ~start:start1 ~stop:stop1 () 
  |> Array.map (fun x ->
      solve2 ~start:(start2,start3) ~stop:(stop2,stop3) (f x)
      |> Array.map (fun (y,z) -> (x,y,z)))
  |> Array.to_list
  |> Array.concat
  *)
 
let opt2 t ~nb_runs ~fr_start ~fr_stop ~a_start ~a_stop pr =
  let f = f t in
  let tc1 = { float_range = fr_start
            ; accuracy = a_start
            ; nb_runs
            ; pass_rate = pr
            }
  in
  solvei2 (fun float_range accuracy -> f {tc1 with float_range; accuracy})
    ~start:(fr_start,a_start) ~stop:(fr_stop,a_stop)

