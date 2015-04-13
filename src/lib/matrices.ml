
open Util

let add x y = Array.init (Array.length x) (fun i -> x.(i) +. y.(i))

let dot x y = Array.fold2 (fun s x y -> s +. x *. y) 0.0 x y
