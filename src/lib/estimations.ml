
open Util

(* the trick to create dx2 tries to ensure that dx2 is always a numerically representable number. *)
let secant f =
  let dx  = 2.22044e-16 in
  let h x = x *. sqrt dx in
  (fun x ->
    let x_i = x +. h x in
    let dx2 = x_i -. x in
    (f (x +. dx2) -. f x) /. dx2)

(* a three point estimate of the numerical derivative of a function. *)
let three_point_est f =
  let dx  = 2.22044e-16 in
  let h x = x *. sqrt dx in
  (fun x ->
    let x_i = x +. (h x) in
    let h_1 = x_i -. x in
    let x_j = x -. (h x) in
    let h_2 = x -. x_j in
    (f (x +. h_1) -. f (x -. h_2)) /. (h_1 +. h_2))

let newton_raphson_method_full f df ?init ~accuracy ~iterations ~lower_bound ~upper_bound =
  let rec loop x i =
(*  printf "i %d: x %f\n" i x; *)
    if x < lower_bound then
      raise (IterationFailure ("newto_raphson", OutOfBounds lower_bound))
    else if x > upper_bound then
      raise (IterationFailure ("newto_raphson", OutOfBounds upper_bound))
    else if i > iterations then
      raise (IterationFailure ("newto_raphson", TooManyIterations i))
    else
      let y = f x in
      if abs_float y < accuracy then
        x
      else
        let x_n = x -. (y /. (df x)) in
        loop x_n (i + 1)
  in
  match init with
  | None   -> loop (midpoint upper_bound lower_bound) 0
  | Some i -> loop i 0

let newton f ~init =
  newton_raphson_method_full f (three_point_est f) ~init
      ~accuracy:1.0e-10 ~iterations:1000

let bisection f ~epsilon ~lower_bound ~upper_bound =
  let rec loop lb ub =
(*  printf "lb %.5f ub %.5f\n" lb ub; *)
    let inside l u = (l < 0.0 && u > 0.0) || (l > 0.0 && u < 0.0) in
    let mp = midpoint lb ub in
      if (abs_float (ub -. lb)) < 2.0 *. epsilon then (* fin *)
        mp
      else
        let flb = f lb in
        let fmp = f mp in
        let fub = f ub in
        if flb = 0.0 then
          lb
        else if fmp = 0.0 then
          mp
        else if fub = 0.0 then
          ub
        else if inside flb fmp then (* go left! *)
          loop lb mp
        else if inside fmp fub then (* go right! *)
          loop mp ub
        (* if the user does not provide an interval where the function has an intersection. *)
        else if flb < 0.0 && fub < 0.0 then
          lb
        (* if the user does not provide an interval where the function has an intersection. *)
        else if flb > 0.0 && fub > 0.0 then
          ub
        else (* close enough. *)
          mp
(*  this method is commented out because flb * fmp might be unstable, better to compare directly.
            let fmp = f mp in
            if ((f lb) * fmp) < 0.0 then (* go left! *)
                loop lb mp
            else if (fmp * (f ub)) < 0.0 then (* go right! *)
                loop mp ub
            else (* got it *)
                mp *)
    in
    loop lower_bound upper_bound
