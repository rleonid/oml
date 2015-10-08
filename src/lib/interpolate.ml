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

module Tri_Diagonal = struct

  let mult m x =
    let n = Array.length m in
    Array.init n (fun i ->
      let a, b, c = m.(i) in
      if i = 0 then
        b *. x.(0) +. c *. x.(1)
      else if i = n - 1 then
        a *. x.(i-1) +. b *. x.(i)
      else
        a *. x.(i-1) +. b *. x.(i) +. c *. x.(i+1))

  (* Solve a system of eqautions of the form
    a_i*x_{i-1} + b_i*x_i + c_i*x_{i+1} = d_i, where a_1 = c_n = 0.
    The first arr of triples is of (a_i, b_i, c_i) and a_0 and c_n are ignored.
    The second arr of d.

    This probably has uses outside of spline interpolation.
    TODO: Now that we're linking against Lacaml, makes sense to test if using
    lapack improves performance or precision.*)
  let solve r d =
    let a i = fst3 (r.(i)) in
    let b i = snd3 (r.(i)) in
    let c i = thr3 (r.(i)) in
    let n = Array.length r in
    if n = 1 then
      [| d.(0) /. (b 0) |]
    else
      let nm1 = n - 1 in
      let cm = Array.make nm1 nan in
      let dm = Array.make nm1 nan in
      for i = 0 to n - 2 do
        if i = 0 then begin
          cm.(i) <- (c i) /. (b i);
          dm.(i) <- d.(i) /. (b i)
        end else begin
          let im1 = i - 1 in
          cm.(i) <- (c i) /. ((b i) -. cm.(im1) *. (a i));
          dm.(i) <- (d.(i) -. dm.(im1) *. (a i)) /. ((b i) -. cm.(im1) *. (a i))
        end
      done;
      let x = Array.make n nan in
      x.(nm1) <- (d.(nm1) -. dm.(nm1-1) *. (a nm1))
                /. ((b nm1)-.(cm.(nm1-1))*.(a nm1));
      for i = n - 2 downto 0 do
        x.(i) <- dm.(i) -. cm.(i) *. x.(i+1)
      done;
      x
 
end (* Tri_Diagonal *)
 
module Spline = struct

  open Tri_Diagonal
 
  type boundary_condition =
    | Natural
    | Clamped of float * float

  type t = (float * float * (float * float * float) option) array

  let knots = Array.map (fun (x,y,_) -> (x,y))

  let coefficients t =
    Array.init (Array.length t - 1)
      (fun i ->
        let x, y, opt = t.(i) in
        match opt with
        | None -> invalidArg "Improper spline size %d" i
        | Some (c, b, a)  -> y, c, b, a)

  (* Helper functions to extract correctly indexed elements of data.
     TODO: is this currying worthwhile?
  *)
  let h arr i =
    fst arr.(i + 1) -. fst arr.(i)

  let s arr =
    let h = h arr in
    (fun i -> (snd arr.(i + 1) -. snd arr.(i)) /. (h i))

  let v arr = 
    let h = h arr in
    (fun i -> 2. *. ((h (i-1)) +. (h i)))

  let u arr =
    let s = s arr in
    (fun i -> 6. *. ((s i) -. (s (i-1)))) 

  let setup_tridiagonal arr =
    let h = h arr in
    let v = v arr in
    let u = u arr in
    let n = Array.length arr in
    let abc_s = n - 2 in
    let d = Array.init abc_s (fun i -> u (i + 1)) in
    let abc = Array.init abc_s (fun i ->
      let ip1 = i + 1 in
      let a = if ip1 = 1 then nan else (h i) in
      let c = if ip1 = abc_s then nan else (h ip1) in
      a, (v ip1), c)
    in
    abc, d

  let fit ?(bc=Natural) arr =
    let n = Array.length arr in
    if n < 3 then
      invalidArg "Less than 3 data points %d" n;
    Array.sort (fun (x1,_) (x2,_) -> compare x1 x2) arr;
    let m =
      let abc, d = setup_tridiagonal arr in
      (* Modify the end points based upon boundary condition. *)
      match bc with
      | Natural ->
        let m_nm2 = Tri_Diagonal.solve abc d in
        let m = Array.make n 0. in
        Array.blit m_nm2 0 m 1 (n - 2);
        m
      | Clamped (m_0, m_n) ->
          failwith "Not implemented"
    in
    let h = h arr in
    let s = s arr in
    Array.mapi (fun i (x,y) ->
      if i = n - 1 then
        (x,y,None)
      else
        let a = (m.(i+1) -. m.(i)) /. (6. *. (h i)) in             (* 3rd order. *)
        let b = m.(i) /. 2. in                                     (* 2nd order. *)
        let c = (s i) -. (h i) *. (2. *. m.(i) +. m.(i+1))/.6.0 in (* linear term. *)
        x, y, Some (c, b, a))
      arr

  let rec eval_at t i x =
    let x_i, d, opt = t.(i) in
    if x_i = x then
      d
    else
      match opt with
      | None           ->
          eval_at t (i - 1) x (* Use previous point where last cubic is stored*)
      | Some (c, b, a) ->
        let xd = x -. x_i in
        (*unroll: Float.(a * (xd ** 3.0) + b * (xd ** 2.0) + c * xd + d) *)
        Float.(((a * xd + b) * xd + c) * xd + d)

  let eval t x =
    let idx = Array.binary_search (fun (x1,_,_) -> compare x x1) t in
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
          let x_i,_,_ = t.(s_i) in
          if x < x_i then begin
            r.(v_i) <- eval_at t ei x;
            loop (v_i + 1) s_i
          end else
            loop v_i (s_i + 1)
    in
    loop 0 0

end (* Spline *)

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


