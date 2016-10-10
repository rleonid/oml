
(* 
corebuild -package lacaml,core_bench -I src/scripts/prof centering_test.native
*)

open Lacaml.D
open Core.Std
open Core_bench.Std

(* Allocates 2w more but is more than 2x faster.*)
let centering_ger n =
  let i = Mat.identity n in
  let o = Vec.make n 1. in
  ger ~alpha:(-1./. (float n)) o o i

let centering_init n =
  let f = float n in
  let d = (f-. 1.) /. f in
  let o = -1./. f in
  Mat.init_rows n n (fun i j -> if i = j then d else o)

let () =
  Command.run (Bench.make_command
    [ Bench.Test.create ~name:"using ger 100"
        (fun () -> ignore (centering_ger 100))
    ; Bench.Test.create ~name:"using init 100"
        (fun () -> ignore (centering_init 100))
    ; Bench.Test.create ~name:"using ger 1000"
        (fun () -> ignore (centering_ger 1000))
    ; Bench.Test.create ~name:"using init 1000"
        (fun () -> ignore (centering_init 1000))
    ; Bench.Test.create ~name:"using ger 10000"
        (fun () -> ignore (centering_ger 10000))
    ; Bench.Test.create ~name:"using init 10000"
        (fun () -> ignore (centering_init 10000))
    ])

