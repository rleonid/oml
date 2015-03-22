
(* create an array of data correlated (by r) with the first array. *)
let correlated r ar1 ar2 =
  Array.map2 (fun x1 x2 -> r *. x1 +. (sqrt (1.0 -. r *. r)) *. x2) ar1 ar2

(*
val correlated : float -> float array -> float array -> float
            array
            *)
