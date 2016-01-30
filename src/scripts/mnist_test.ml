(*
  #require "dsfo";;
  -- or --
  ocamlbuild -use-ocamlfind -pkgs dsfo,lacaml,ocephes,lbfgs -I src/scripts/ -I src/lib/ mnist_test.native

   expected results:
   train 0.934 correct
   test 0.926 correct
*)
open Lacaml.D

let feature_size = 28 * 28

module MnistEncoded =
  struct
    type clas = int
    type feature = float array
    let encoding arr = arr
    let size = feature_size
  end

open Oml.Classification
module P = Probabilities
module MnistLr = Logistic_regression.Multiclass(MnistEncoded)

let m_train = Mnist.data ~dir:"../dsfo" `Train
let m_test = Mnist.data ~dir:"../dsfo" `Test

let column_to_label c =
  let rec loop i =
    if c.{i + feature_size} = 1.0 then i
    else loop (i + 1) in
  loop 1

let to_samples data =
  Array.init (Mat.dim2 data) (fun i ->
    let col = Mat.col data (i + 1) in
    let ftr = copy ~n:MnistEncoded.size col |> Vec.to_array in
    let lab = column_to_label col in
    lab, ftr)
  |> Array.to_list

let s_train = to_samples m_train

let tolerance =
  if not (!Sys.interactive) && Array.length Sys.argv = 2 then
    float_of_string Sys.argv.(1)
  else
    1e7

let mnist_lr = MnistLr.(estimate ~opt:(opt ~tolerance ()) s_train)

let perf =
  s_train
  |> List.map (fun (c, f) -> c, P.most_likely (MnistLr.eval mnist_lr f))

let correct = List.fold_left (fun c (a,p) -> if a = p then c + 1 else c) 0 perf
let total = List.length perf
let () = Printf.printf "train %0.3f correct\n" ((float correct) /. (float total))

let s_test = to_samples m_test

let perf_test =
  s_test
  |> List.map (fun (c, f) -> c, P.most_likely (MnistLr.eval mnist_lr f))

let correct_test = List.fold_left (fun c (a,p) -> if a = p then c + 1 else c) 0 perf_test
let total_test = List.length perf_test
let () =
  Printf.printf "test %0.3f correct\n"
    ((float correct_test) /. (float total_test))

