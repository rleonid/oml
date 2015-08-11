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

(* create an array of data correlated (by r) with the first array. *)
let correlated r ar1 ar2 =
  Array.map2 (fun x1 x2 -> r *. x1 +. (sqrt (1.0 -. r *. r)) *. x2) ar1 ar2

(*
val correlated : float -> float array -> float array -> float
            array
            *)
