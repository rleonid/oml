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
module F = Uncategorized.Functions
let invalid_arg ~f fmt = invalid_arg ~m:"Sampling" ~f fmt

type 'a generator = unit -> 'a

let init = function
  | None   -> Random.State.make_self_init ()
  | Some a -> Random.State.make a

let int_upper_bound = 2 lsl (30 - 1)

let uniform_i ?seed n =
  if n <= 0 || n >= int_upper_bound then invalid_arg ~f:"uniform_i" "n" else
  let r = init seed in
  fun () -> Random.State.int r n

let uniform_f ?seed n =
  if n <= 0.0 then invalid_arg ~f:"uniform_f" "n" else
  let r = init seed in
  fun () -> Random.State.float r n

let normal_std ?seed () =
  let r = init seed in
  let cur_ref = ref None in
  (fun () ->
    let p = fun () -> 2.0 *. (Random.State.float r 1.0) -. 1.0 in
    let rec loop v1 v2  =
      let rsq = v1 *. v1 +. v2 *. v2 in
      if rsq >= 1.0 || rsq = 0.0 then
        loop (p ()) (p ())
      else
        let fac = sqrt ( -2.0 *. (log rsq) /. rsq) in
        cur_ref := Some (v2 *. fac) ;
        v1 *. fac
    in
    match !cur_ref with
    | Some x -> (cur_ref := None; x)
    | None   -> loop (p ()) (p ()))

let normal ?seed ~mean ~std () =
  if std < 0.0 then invalid_arg ~f:"normal" "std" else
  if std = 0.0 then (fun () -> mean) else
  let rsn = normal_std ?seed () in
  (fun () -> std *. (rsn ()) +. mean)

(** {{:http://www.keithschwarz.com/darts-dice-coins} Alias method} for
    sampling from a Categorical distribution.

    The method allows to generate a sample in costant time after a
    linear time preprocessing of the probabilities array. *)
module Alias =
  struct
    type t =
      { n : int
      ; probs: float array
      ; alias: int array
      }

    let create weights =
      (* [Alias] is currently internal to [Sampling], therefore
         validating [weights] is a responsibility of the caller. *)
      let weights = Array.copy weights in
      let n = Array.length weights in
      let average = 1.0 /. float n in

      let rec iter weights probs alias = function
      | ([], rest) | (rest, []) ->
        List.iter (fun lg -> probs.(lg) <- 1.0) rest
      | (l::small, g::large) -> begin
          probs.(l) <- weights.(l) *. float n;
          alias.(l) <- g;

          weights.(g) <- (weights.(g) +. weights.(l)) -. average;
          if weights.(g) < average then
            iter weights probs alias (g::small, large)
          else
            iter weights probs alias (small, g::large)
        end
      in

      let rec partition weights i (small, large) =
        if i = n then
          (small, large)
        else if weights.(i) < average then
          partition weights (succ i) (i::small, large)
        else
          partition weights (succ i) (small, i::large)
      in

      let (small, large) = partition weights 0 ([], []) in
      let probs = Array.make n 0.0 in
      let alias = Array.make n 0 in begin
        iter weights probs alias (small, large);
        { n; probs; alias }
      end

    let sample { n; probs; alias } r =
      let i = Random.State.int r n in
      if (Random.State.float r 1.0 < probs.(i)) then i else alias.(i)
  end

let categorical ?seed weights =
  if not (Array.all (within (Open 0.0, Closed 1.0)) weights) then
    invalid_arg ~f:"categorical" "weights must be within (0, 1]"
  else
    let sum = Array.sumf weights in
    if significantly_different_from 1.0 sum then
      invalid_arg ~f:"categorical" "weights must sum to 1 (got: %.2f)" sum
    else
      let r = init seed
      and alias = Alias.create weights in
      (fun () -> Alias.sample alias r)

let softmax ?seed ?temperature weights =
  categorical ?seed (F.softmax ?temperature weights)


module Poly =
  struct

    let invalid_arg ~f fmt = Util.invalid_arg ~m:"Poly.Sampling" ~f fmt

    let uniform ?seed elems =
      let f = uniform_i ?seed (Array.length elems) in
      fun () -> elems.(f())

    let categorical ?seed elems weights =
      if (Array.length elems <> Array.length weights) then
        invalid_arg ~f:"categorical" "weights" else
          let f = categorical ?seed weights in
          fun () -> elems.(f())

    let softmax ?seed ?temperature elems weights =
      if (Array.length elems <> Array.length weights) then
        invalid_arg ~f:"softmax" "weights" else
          let f = softmax ?seed ?temperature weights in
          fun () -> elems.(f())
  end
