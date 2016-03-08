(* 
  corebuild -package lacaml,core_bench -I src/scripts/prof scatter_test.native
*)

open Lacaml.D
open Core_bench.Std

let centering_ger n =
  let i = Mat.identity n in
  let o = Vec.make n 1. in
  ger ~alpha:(-1./. (float n)) o o i

(* Using data on a per row basis *)
(* Comparing THIS *)
let scatter_2gemm a =
  let n = Mat.dim1 a in
  let c = centering_ger n in
  gemm (gemm ~transa:`T a c) a

let col_mean n c = Vec.sum c /. n

let column_means a =
  let n = Mat.dim1 a in
  gemv ~alpha:(1./. (float n)) ~trans:`T a (Vec.make n 1.)

(* vs THIS *)
let scatter_fold_ger a =
  let n = Mat.dim1 a in
  let u = column_means a in
  ger ~alpha:(-1. *. float n) u u (gemm ~transa:`T a a)

let () =
  let hun = Mat.random 100 100 in
  let tho = Mat.random 1000 1000 in
  let ten = Mat.random 10000 10000 in
  Core.Command.run (Bench.make_command
    [ Bench.Test.create ~name:"using 2gemm 100"
        (fun () -> ignore (scatter_2gemm hun))
    ; Bench.Test.create ~name:"using fold_ger 100"
        (fun () -> ignore (scatter_fold_ger hun))
    ; Bench.Test.create ~name:"using 2gemm 1000"
        (fun () -> ignore (scatter_2gemm tho))
    ; Bench.Test.create ~name:"using fold_ger 1000"
        (fun () -> ignore (scatter_fold_ger tho))
    ; Bench.Test.create ~name:"using 2gemm 10000"
        (fun () -> ignore (scatter_2gemm ten))
    ; Bench.Test.create ~name:"using fold_ger 10000"
        (fun () -> ignore (scatter_fold_ger ten))
    ])
