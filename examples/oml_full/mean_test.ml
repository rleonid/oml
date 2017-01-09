open Oml_full
open Statistics

let mean = 2.
let std = 1.
let gen = Sampling.normal ~mean ~std () ;;
let samples = 100

let data = Array.init samples (fun _ -> gen ()) ;;
let test_value = 1.9
let tr = Hypothesis_test.mean_t_test test_value Hypothesis_test.Two_sided data ;;

let () =
  if !Sys.interactive then () else
    Printf.printf "Our data is %d samples with a mean of %f and a std of %f, \
          if we test for a mean of %f \n\
          the prob of seeing this result by chance is: %f\n"
      samples mean std test_value tr.Hypothesis_test.prob_by_chance
