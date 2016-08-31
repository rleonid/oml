(*
   Copyright 2016
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

(** See message above add_lite_ml_and_mlt_and_depends for explanation. *)
if Filename.basename Sys.argv.(0) <> "omltest.native" then
  failwith ("Linked the Functions tests stub file into: " ^ Sys.argv.(0))

include Oml_naive_bayes
