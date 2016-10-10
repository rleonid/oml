(*
   Copyright 2016:
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

(** Kahan summation. *)

type t = { correction : float; sum : float }

let empty = { correction = 0.; sum = 0. }

let zero = empty

let update t v =
  let x  = v -. t.correction in
  let ns = t.sum +. x in
  { correction = (ns -. t.sum) -. x
  ; sum = ns
  }

let ( + ) = update

let sum t = t.sum
