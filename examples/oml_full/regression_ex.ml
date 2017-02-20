open Oml_full
open Statistics
open Regression

let rows = 100
let cols = 3

let gen = Sampling.normal_std () ;;
let pred = Array.init rows (fun _ -> Array.init cols (fun _ -> gen ())) ;;
let constant_term = 2.
let a1 = 3.
let a2 = 4.
let a3 = 5.
let resp =
  Array.map (fun row ->
    constant_term +. a1 *. row.(0) +. a2 *. row.(1) +. a3 *. row.(2)) pred ;;

let opt = (* Redundant, just for examples *)
  Multivariate.opt ~add_constant_column:true
    () ;;

let t = Multivariate.regress ~opt pred ~resp ;;
let c = Multivariate.coefficients t ;;

let () =
  if !Sys.interactive then () else
    Printf.printf "Regression a linear model of y = %f + %f * x1 + %f * x2 + %f * x3 \n\
      over randomly generated input data, our regression coefficients are: \n\
        %f %f %f %f."
        constant_term a1 a2 a3 c.(0) c.(1) c.(2) c.(3)
