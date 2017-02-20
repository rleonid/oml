open Oml.Util
open Oml.Statistics

let n1 = Sampling.normal ~mean:2. ~std:1. ()
let n2 = Sampling.normal ~mean:2. ~std:10. ()
let samples = 10000
let data = Array.init samples (fun i -> if i mod 10 = 0 then n2 () else n1 ())
let kurtosis = Descriptive.classify_kurtosis data

let () =
  if !Sys.interactive then () else
    Printf.printf "Our data has %s kurtosis\t (expecting fat).\n"
      (match kurtosis with
        | `Fat -> "fat"
        | `Normal -> "normal"
        | `Skinny -> "skinny"
        | `Slightly_fat -> "slightly fat"
        | `Slightly_skinny -> "slightly skinny")
