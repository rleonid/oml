
open Util

type t = float array

(* TODO: add range checks. *)
let equal ?d x y =
  let nsdf a b = not (Util.significantly_different_from ?d a b) in
  let n = Array.length x
  and m = Array.length y in
  let rec loop i =
    i = n || nsdf x.(i) y.(i) && loop (i + 1)
  in
  n = m && loop 0

let add x y = Array.init (Array.length x) (fun i -> x.(i) +. y.(i))
let sub x y = Array.init (Array.length x) (fun i -> x.(i) -. y.(i))

let mult x  = Array.map (( *.) x)

let dot x y = Array.fold2 (fun s x y -> s +. x *. y) 0.0 x y
