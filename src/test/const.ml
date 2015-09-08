
open Oml

let () =
  Test_utils.debug_ref := true;
  let t = Test_utils.TestMap.find "lrm1" !Test_utils.constrained_map in
  let perf =  Test_utils.opt2 t ~nb_runs:100 ~fr_start:7 ~fr_stop:10 ~a_start:(-3) ~a_stop:0 0.99 in
  Array.iter (fun (fr,a) -> Printf.printf "%d, %d\n%!" fr a) perf
