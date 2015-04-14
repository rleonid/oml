
open Util

type matrix = Vectors.vector array

let equal ?d x y =
  let n = Array.length x
  and m = Array.length y in
  let rec loop i =
    i = n || Vectors.equal ?d x.(i) y.(i) && loop (i + 1)
  in
  n = m && loop 0

let add x y = Array.init (Array.length x) (fun i -> Vectors.add x.(i) y.(i))
let sub x y = Array.init (Array.length x) (fun i -> Vectors.sub x.(i) y.(i))

let mult x = Array.map (Vectors.mult x)
