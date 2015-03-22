
open Util

let random_std_normal ?seed () =
  let r =
    match seed with
    | None   -> Random.State.make_self_init ()
    | Some a -> Random.State.make a
  in
  let cur_ref = ref None in
  (fun () ->
    let p = fun () -> 2.0 *. (Random.State.float r 1.0) -. 1.0 in
    let rec loop v1 v2  =
      let rsq = v1 *. v1 +. v2 *. v2 in
      if rsq >= 1.0 || rsq = 0.0 then
        loop (p ()) (p ())
      else
        let fac = sqrt ( -2.0 *. (log rsq) /. rsq) in
        cur_ref := Some (v2 *. fac) ;
        v1 *. fac
    in
    match !cur_ref with
    | Some x -> (cur_ref := None; x)
    | None   -> loop (p ()) (p ()))

let random_normal ?seed ~mean ~std () =
  let rsn = random_std_normal ?seed () in
  (fun () -> std *. (rsn ()) +. mean)


(* create an array of data correlated (by r) with the first array. *)
let correlated_random r ar1 ar2 =
  Array.map2 (fun x1 x2 -> r *. x1 +. (sqrt (1.0 -. r *. r)) *. x2) ar1 ar2

