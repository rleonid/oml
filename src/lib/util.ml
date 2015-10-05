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

type iterative_failure_reason =
    | Out_of_bounds of float
    | No_convergence
    | Too_many_iterations of int
    | Too_few_iterations of int

exception Iteration_failure of string * iterative_failure_reason

let invalidArg fmt = Printf.ksprintf (fun s -> raise (Invalid_argument s)) fmt
let pi = 4. *. atan 1.

module Array = struct
  include Array

  let fold2 f i a b =
    let n = Array.length a
    and m = Array.length b in
    if n <> m then
      invalidArg "unequal lengths %d and %d" n m
    else
      begin
        let r = ref i in
        for i = 0 to n - 1 do
          r := f !r a.(i) b.(i)
        done;
        !r
      end

  let map2 f a b =
    let n = Array.length a
    and m = Array.length b in
    if n <> m then
      invalidArg "unequal lengths %d and %d" n m
    else
      Array.mapi (fun i a_i -> f a_i b.(i)) a

  (* TODO: On the subject of heurisitcs, see Kahan's summation algorithm. *)
  let sumf = Array.fold_left (+.) 0.0

  (* TODO: Are there heuristics to consider when arrays are large? or elements
    in the array are particulary large/small. log transforms?
    Specifically, if I'm not going to use this method for geometric_mean,
    when should I use this default version?  *)
  let prodf = Array.fold_left ( *. ) 1.0

  let max a = Array.fold_left max a.(0) a
  let min a = Array.fold_left min a.(0) a

  let find_index f a =
    let n = Array.length a in
    let rec loop i =
      if i >= n then
        raise Not_found
      else if f a.(i) then
        i
      else
        loop (i + 1)
    in
    loop 0

  let bs precise c a =
    let rec bs_loop mi mx =
      if mx < mi then
        if precise then
          raise Not_found
        else
          mx
      else
        let md = (mx + mi) / 2 in
        let cc = c a.(md) in
        if cc < 0
        then bs_loop mi (md - 1)
        else if cc > 0
            then bs_loop (md + 1) mx
            else md
    in
    bs_loop 0 (Array.length a - 1)

  let binary_search c arr = bs false c arr
  let binary_search_exn c arr = bs true c arr

  let all p a =
    let n = Array.length a in
    let rec loop i =
      i = n || (p a.(i)) && loop (i + 1)
    in
    loop 0

  let any p a =
    let n = Array.length a in
    let rec loop i =
      if i < n
      then (p a.(i)) || loop (i + 1)
      else false
    in
    loop 0

  let has_order c a =
    let n = Array.length a in
    let rec loop i =
      i = n || (c a.(i -1) a.(i) && loop (i + 1))
    in
    loop 1

  let range ?(incr=1.0) ~start ~stop () =
    if stop < start
    then [||]
    else
      Array.init (truncate (ceil ((stop -. start) /. incr)))
        (fun i -> start +. incr *. (float i))

  let ranks = Rank.ranks
end

let midpoint x y = (x +. y) /. 2.0

(* This value is taken from http://en.wikipedia.org/wiki/Machine_epsilon which
    sites Higham, Nicholas (2002). Accuracy and Stability of Numerical Algorithms
    (2 ed). SIAM. p. 37. *)
let dx = 2.22044e-16

let significantly_different_from ?(d=dx) x y = y < (x -. d) || y > (x +. d)

let equal_floats ~d x y = not (significantly_different_from ~d x y)

let is_nan x = x <> x

let is_degenerate x = is_nan x || x = neg_infinity || x = infinity

type 'a bound = Open of 'a
              | Closed of 'a

let within bp x =
  match bp with
  | (Open l), (Open u)      -> l < x  && x < u
  | (Open l), (Closed u)    -> l < x  && x <= u
  | (Closed l), (Open u)    -> l <= x && x < u
  | (Closed l), (Closed u)  -> l <= x && x <= u


module Float = struct
  let ( + ) x y = x +. y
  let ( - ) x y = x -. y
  let ( * ) x y = x *. y
  let ( / ) x y = x /. y
end

module type Optional_arg_intf = sig

  type spec             (** type of default argument. *)
  val default : spec    (** A default value used when not specified.*)
end


