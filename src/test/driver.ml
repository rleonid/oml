
open Kaputt.Abbreviations
open Oml
module Rank = Rank  (* TODO: figure out why this needs to referenced. *)

let () =
  Printf.printf "Running test!\n";
  Test.launch_tests ();
  Printf.printf "Finished running linked tests!\n"
