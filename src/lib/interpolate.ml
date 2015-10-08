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

open Util

let linear (ax, ay) (bx, by) =
  if ax = bx then
    invalidArg "equal x values %f" ax
  else
    let slope = Float.((by - ay) / (bx - ax)) in
    fun x -> Float.(slope * (x - ax) + ay)
   
(* Solve a system of eqautions of the form
   a_i*x_{i-1} + b_i*x_i + c_i*x_{i+1} = d_i, where a_1 = c_n = 0
   The first arr of triples is of (a_i, b_i, c_i) and a_0 and c_n are ignored.
   The second arr of d *)
let solve_tri_diagonal m_arr d =
  let a = Array.map fst3 m_arr in
  let b = Array.map snd3 m_arr in
  let c = Array.map thr3 m_arr in
  let n = Array.length m_arr in
  let cm = Array.make (n - 1) nan in
  let dm = Array.make n nan in
  for i = 0 to n - 2 do
    if i = 0 then begin
      cm.(i) <- c.(i) /. b.(i);
      dm.(i) <- d.(i) /. b.(i)
    end else begin
      let im1 = i - 1 in
      cm.(i) <- c.(i) /. (b.(i) -. cm.(im1) *. a.(i));
      dm.(i) <- Float.((d.(i) - dm.(im1) * a.(i)) / (b.(i) - cm.(im1) * a.(i)))
    end
  done;
  let x = Array.make n nan in
  x.(n - 1) <- (d.(n-1) -. dm.(n-2) *. a.(n-1)) /. (b.(n-1)-.c.(n-2)*.a.(n-1));
  for i = n - 2 downto 0 do
    x.(i) <- dm.(i) -. c.(i) *. x.(i+1)
  done;
  x

module Spline = struct

  type cubic_spline_boundary_condition =
    | NaturalCubic      (* The 2nd derivatives at the end points are 0. S''(x_0) = S''(x_n) = 0. *)
    | Clamped           (* The 1st derivatives at the end points of the spline are equal to the passed functions. S'(x_0) = f'(x_0) && S'(x_n) = f'(x_n). *)
          
  type t = (float * float * float * float * float) array

  let eval_at t i x =
    let x_i, d, c, b, a = t.(i) in
    if x_i = x then
      d
    else
      let xd = x -. x_i in
      Float.(a * (xd ** 3.0) + b * (xd ** 2.0) + c * xd + d)

  let eval t x =
    let idx = Array.binary_search (fun (x1,_,_,_,_) -> compare x x1) t in
    let idx = max 0 idx in
    eval_at t idx x

  let eval_arr t arr =
    let n = Array.length t in
    let m = Array.length arr in
    let r = Array.make m 0.0 in
    let rec loop v_i s_i =
      if v_i >= m then
        r
      else
        let x = arr.(v_i) in
        let ei = max 0 (s_i - 1) in 
        if s_i >= n then begin
          r.(v_i) <- eval_at t ei x;
          loop (v_i + 1) s_i
        end else
          let x_i, _, _, _, _ = t.(s_i) in
          if x < x_i then begin
            r.(v_i) <- eval_at t ei x;
            loop (v_i + 1) s_i
          end else
            loop v_i (s_i + 1)
    in
    loop 0 0

  let fit ?(sorted=false) bc arr =
    if not sorted then
      Array.sort (fun (x1,_) (x2,_) -> compare x1 x2) arr;
    let x_arr = Array.map fst arr in
    let y_arr = Array.map snd arr in
    let n     = Array.length arr in
    let h i   = x_arr.(i+1) -. x_arr.(i) in
    let s i   = (y_arr.(i+1) -. y_arr.(i)) /. (h i) in
    let v i   = 2. *. ((h (i-1)) +. (h i)) in
    let u i   = 6. *. ((s i) -. (s (i-1))) in
    let d_arr = Array.init (n - 2) (fun i -> u (i + 1)) in
    let m_arr = Array.init (n - 2) (fun i ->
      let ip1 = i + 1 in
      if ip1 = 1 then
        nan, (v ip1), (h ip1)
      else if ip1 = n - 1 then
        (h i), (v ip1), nan
      else
        (h i), (v ip1), (h ip1))
    in
    let z_arr = solve_tri_diagonal m_arr d_arr in
    let zz = Array.make n 0. in
    Array.blit z_arr 0 zz 1 (n - 2);
    (* Modify the end points based upon boundary condition. *)
    Array.init (n - 2) (fun i -> 
      let a = (zz.(i+1) -. zz.(i)) /. (6. *. (h i)) in             (* 3rd order. *)
      let b = zz.(i) /. 2. in                                      (* 2nd order. *)
      let c = (s i) -. (h i) *. (2. *. zz.(i) +. zz.(i+1))/.6.0 in (* linear term. *)
      x_arr.(i), y_arr.(i), c, b, a)
           
  let lagrange arr =
    let wi_arr = Array.mapi (fun idx (x, y) -> (idx, x, y)) arr in
    (* compute the product across all of the terms, excluding a given index. *)
    let xProd j x = 
      Array.fold_left (fun den (idx, x_i, _y_i) ->
        if idx = j then
            den
          else
            (x -. x_i) *. den) 1.0 wi_arr
    in
    let l_j_arr =
      Array.map (fun (j, x_j, y_j) ->
        let den = xProd j x_j in
        let l_j x = (xProd j x) /. den in
        y_j, l_j) wi_arr
    in
    (fun x ->
      Array.fold_left(fun s (y_j, l_j_f) -> Float.(x + y_j * (l_j_f x))) 0.0 l_j_arr)

end (* Spline *)
