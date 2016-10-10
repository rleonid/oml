(*
  corebuild -package lacaml,core_bench -I src/scripts/prof column_mean_test.native
*)

open Lacaml.D
open Core_bench.Std

let col_mean n c = Vec.sum c /. n

(* Test THIS  *)
let column_mean_init a =
  let n = float (Mat.dim1 a) in
  let m = Mat.dim2 a in
  Vec.init m (fun i -> col_mean n (Mat.col a i))

(* vs THIS *)
let column_mean_gemv a =
  let n = Mat.dim1 a in
  gemv ~alpha:(1./. (float n)) ~trans:`T a (Vec.make n 1.)
(* No brainer that this is better*)

let () =
  let mr n = Mat.random n n in
  let tens = mr 10 in
  let huns = mr 100 in
  let thou = mr 1000 in
  let teth = mr 10000 in
  Core.Command.run (Bench.make_command
    [ Bench.Test.create ~name:"Using init 10"
      (fun () -> ignore(column_mean_init tens))
    ; Bench.Test.create ~name:"Using gemv 10"
      (fun () -> ignore (column_mean_gemv tens))
    ; Bench.Test.create ~name:"Using init 100"
      (fun () -> ignore(column_mean_init huns))
    ; Bench.Test.create ~name:"Using gemv 100"
      (fun () -> ignore (column_mean_gemv huns))
    ; Bench.Test.create ~name:"Using init 1000"
      (fun () -> ignore(column_mean_init thou))
    ; Bench.Test.create ~name:"Using gemv 1000"
      (fun () -> ignore (column_mean_gemv thou))
    ; Bench.Test.create ~name:"Using init 10000"
      (fun () -> ignore(column_mean_init teth))
    ; Bench.Test.create ~name:"Using gemv 10000"
      (fun () -> ignore (column_mean_gemv teth))
    ])
