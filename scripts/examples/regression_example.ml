let pred = Array.init 100 (fun r -> Array.init 3 (fun _ -> n1 ())) ;;
let resp = Array.map (fun arr -> 2. +. 3. *. arr.(0) +. 4. *. arr.(1) +. 5. *. arr.(2)) pred ;;
let regarg = { Regression.add_constant_column = true; Regression.lambda_spec = None} ;;
let t = Regression.Multivariate.regress (Some regarg) pred resp ;;
Regression.Multivariate.coefficients t ;;
