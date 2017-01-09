open Oml
open Statistics

(* TODO: Redo this example with a distribution that better models skew. *)
let samples = 10000
let n1 = Sampling.normal ~mean:2. ~std:1. ()
let n2 = Sampling.normal ~mean:3. ~std:1. ()

let data_skew ftf =
  Array.init samples (fun i -> if i mod ftf = 0 then n2 () else n1())
  |> Descriptive.summary
  |> fun s -> s.Descriptive.skew
  |> snd ;;

let count_big_skew =
  Array.fold_left (fun c s ->
      if s = `Positive || s = `Negative then c + 1 else c) 0 ;;

let test_size = 100 ;;

let t10 = Array.init test_size (fun _ -> data_skew 10) |> count_big_skew ;;
let t2 = Array.init test_size (fun _ -> data_skew 2) |> count_big_skew ;;

let () =
  if !Sys.interactive then () else
    Printf.printf "We observed skewed data %d out of %d when the frequency of drawing from a larger mean was 1/%d.\n\
                   But only %d out of %d when the frequency was 1/%d.\n"
      t10 test_size 10 t2 test_size 2
