
open Util

let newton_raphson_full ?init ~accuracy ~iterations ~lower_bound ~upper_bound f df =
  let rec loop x i =
(*  printf "i %d: x %f\n" i x; *)
    if x < lower_bound then
      raise (Iteration_failure ("newton raphson", Out_of_bounds lower_bound))
    else if x > upper_bound then
      raise (Iteration_failure ("newton raphson", Out_of_bounds upper_bound))
    else if i > iterations then
      raise (Iteration_failure ("newton raphson", Too_many_iterations i))
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

let newton ?init ~lower_bound ~upper_bound f =
  newton_raphson_full ?init ~accuracy:1.0e-10 ~iterations:1000
      ~lower_bound ~upper_bound f (Estimations.second_order f)

(* There is a bug in the implementation...
let bisection ~epsilon ~lower_bound ~upper_bound f =
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
        (* if the user does not provide an interval
            where the function has an intersection. *)
        else if flb < 0.0 && fub < 0.0 then
          lb
        (* if the user does not provide an interval
           where the function has an intersection. *)
        else if flb > 0.0 && fub > 0.0 then
          ub
        else (* close enough. *)
          mp
        (*  this method is commented out because flb * fmp
            might be unstable, better to compare directly.
            let fmp = f mp in
            if ((f lb) * fmp) < 0.0 then (* go left! *)
                loop lb mp
            else if (fmp * (f ub)) < 0.0 then (* go right! *)
                loop mp ub
            else (* got it *)
                mp *)
    in
    loop lower_bound upper_bound
    *)
