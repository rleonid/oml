(*
  #require "dsfo";;
  -- or --
  ocamlbuild -use-ocamlfind -pkgs dsfo,lacaml,ocephes,lbfgs -I src/scripts/ -I src/lib/ mnist_test.native

   expected results:
   train 0.934 correct
   test 0.926 correct
*)

(** To visualize original values

let showme ?(data=m_train) i =
  let open Bigarrayo in
  let x_i, y_i = Mnist.decode data i in
  let x_256 =
    let g = genarray_of_array2 x_i in
    GA.init Int8_unsigned Fortran_layout [|28; 28|]
        (fun i -> truncate (255. *. GA.get g i))
  in
  let label =
    let rec loop i = if y_i.{i} = 1. then i - 1 else loop (i + 1)
    in loop 1
  in
  x_256, label;;

Pp.Toplevel.lsc 14;;
*)

(****** DID YOU REQUIRE #DSFO ?!?
#require "dsfo"
*****)
open Oml
open Classification
open Lacaml.D
module P = Probabilities

let m_train = Mnist.data ~dir:"../dsfo" `Train
let m_test = Mnist.data ~dir:"../dsfo" `Test


(******* End 1st Part  *******)

(*
#show_module_type  Classification.Intf.Continuous_encoded_data ;;
*)

let feature_size = 28 * 28
module MnistEncoded =
  struct
    type clas = int
    type feature = float array
    let encoding arr = arr
    let size = feature_size
  end

(******* End 2nd Part  *******)

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
let s_test = to_samples m_test

let tolerance =
  if not (!Sys.interactive) && Array.length Sys.argv = 2 then
    float_of_string Sys.argv.(1)
  else
    1e7
let measure_perf set classify =
  List.map (fun (c, f) -> c, P.most_likely (classify f)) set

let evaluate label classify =
  Printf.printf "%s\n" label;
  let perf = measure_perf s_train classify in
  let correct =
    List.fold_left (fun c (a,p) -> if a = p then c + 1 else c) 0 perf in
  let total = List.length perf in
  Printf.printf "train %0.3f correct\n" ((float correct) /. (float total));
  let perf_test = measure_perf s_test classify in
  let correct_test =
    List.fold_left (fun c (a,p) -> if a = p then c + 1 else c) 0 perf_test in
  let total_test = List.length perf_test in
  Printf.printf "test %0.3f correct\n"
    ((float correct_test) /. (float total_test))

(****** End 3rd Part ****)

module MnistLr = Logistic_regression.Multiclass(MnistEncoded)

let mnist_lr = MnistLr.(estimate ~opt:(opt ~tolerance ()) s_train)
let () = evaluate "logistic regression" (MnistLr.eval mnist_lr)

(****** End 4th Part *****)

module MnistNB = Naive_bayes.Gaussian(MnistEncoded)

let mnist_nb = MnistNB.(estimate s_train)
let () = evaluate "Gaussian naive bayes" (MnistNB.eval mnist_nb)

(*** How about LDA? *)
module MnistLDA = Descriminant.LDA(MnistEncoded)

(* A lot of the features, in this case, pixes in the 28x28 image are always
   0, so they have no variance and this makes the cov-matrix singular. The
   calculation of the inverse of the covariance matrix cannot succeed without
   shrinkage (the determinant will still underflow to 0.0, it was always
   going to be useless, 0.01 ** 780 -> 1e-700 = 0. is unrepresentable ).
   So the probabilities will evaluate to nan's after division by zero, unless
   we apply a little shrinkage, and use that. *)
let mnist_lda = MnistLDA.(estimate s_train ~opt:(opt ~shrinkage:0.01 ()))

let () = evaluate "LDA" (MnistLDA.eval mnist_lda)

(* We need a better feature representation.
  Let's remove all of the columns with zero variance*)

module LU = Uncategorized.Lacaml_util
let x = lacpy ~m:784 m_train
let a = Mat.transpose_copy x
let c = LU.sample_covariance a
let bad_cols =
  Mat.copy_diag c
  |> Vec.fold (fun (i,a) v -> if v = 0. then (i+1, i::a) else (i+1,a)) (1,[])
  |> snd
  |> List.rev

let num_bad_cols = List.length bad_cols

let rec missing n acc = function
  | [] -> List.rev acc
  | (h :: t) as lst ->
      if n < h
      then missing (n + 1) (n :: acc) lst
      else missing (n + 1) acc t

let safe_indices = missing 1 [] bad_cols |> Array.of_list

module MnistEncoded_SubImage =
  struct
    type clas = int
    type feature = float array
    let size = feature_size - num_bad_cols
    let encoding arr = Array.map (Array.get arr) safe_indices
  end

(* Gives identical results... so not that useful? *)
module MnistLDA2 = Descriminant.LDA(MnistEncoded_SubImage)
let mnist_lda2 = MnistLDA2.(estimate ~opt:(opt ~shrinkage:0.01 ()) s_train)
let () = evaluate "Smarter encoded LDA" (MnistLDA2.eval mnist_lda2)

