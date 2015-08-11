(*
   Copyright 2015:
     Leonid Rozenberg <leonidr@gmail.com>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

open Kaputt.Abbreviations
open Oml
module Rank = Rank  (* TODO: figure out why this needs to referenced. *)

let () =
  Printf.printf "Running test!\n";
  Test.launch_tests ();
  Printf.printf "Finished running linked tests!\n"
