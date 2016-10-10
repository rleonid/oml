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

open Oml_util

type t = float array

(* TODO: add range checks. *)
let equal ?d x y =
  let nsdf a b = not (Oml_util.significantly_different_from ?d a b) in
  let n = Array.length x
  and m = Array.length y in
  let rec loop i =
    i = n || nsdf x.(i) y.(i) && loop (i + 1)
  in
  n = m && loop 0

let add x y = Array.init (Array.length x) (fun i -> x.(i) +. y.(i))
let sub x y = Array.init (Array.length x) (fun i -> x.(i) -. y.(i))

let mult x  = Array.map (( *.) x)

let dot x y = Array.fold2 (fun s x y -> s +. x *. y) 0.0 x y
