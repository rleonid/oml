(*
   Copyright 2015:
     Leonid Rozenberg <leonidr@gmail.com>
     Carmelo Piccione <carmelo.piccione@gmail.com>

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

open Util

let small = 1e-50
let max_iteration_default = 200

type t = { get_a : int -> float -> float; get_b : int -> float -> float }

let init ~get_a ~get_b = {get_a;get_b}

let evaluate t ?(epsilon=small) ?(max_iterations=max_iteration_default) x =
  let a = t.get_a 0 0.0 in
  let hPrev =
    (* use the value of small as epsilon criteria for zero checks *)
    if (significantly_different_from ~d:small a 0.0) then a else small in
  let rec loop dPrev cPrev hPrev n : float =
    if n > max_iterations then raise (Iteration_failure
      ("continued fraction failed to stabilize", Too_many_iterations n)) else
      let a = t.get_a n x in
      let b = t.get_b n x in
      let _dN = a +. b *. dPrev in
      let dN = if significantly_different_from
            ~d:small 0.0 _dN then _dN else small in
      let _cN = a +. b /. cPrev in
      let cN = if (significantly_different_from ~d:small 0.0 _cN)
      then _cN else small in
      let dN =  1.0 /. dN in
      let deltaN = cN *. dN in
      let hN = hPrev *. deltaN in
      if hN == infinity then raise (Iteration_failure ("continued fraction diverged", No_convergence)) else
        if (abs_float (deltaN -. 1.0)) < epsilon then
          hN else loop dN cN hN (n+1) in
  let n = 1 in
  let dPrev = 0.0 in
  let cPrev = hPrev in
  let hN = hPrev in
  loop dPrev cPrev hN n

