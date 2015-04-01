
open Kaputt.Abbreviations
open Oml

let () =
  Printf.printf "Running test!\n";
  Test.launch_tests ();
  Printf.printf "Finished running linked tests!\n"
