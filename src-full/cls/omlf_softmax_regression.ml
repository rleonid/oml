(*
   Copyright 2015,2016:
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

open Lacaml.D

(* [general_eval_and_grad ~lambda number_of_classes data_in_rows classes]
   returns a function [f] that for a weight vector [w] and a gradient vector
   [w] of size [number_of_classes * Array.length classes] computes the
   Softmax cost function for [w] and stores the gradient in [g].

   @param classes encodes the different classes as integers starting with 1 to
    [number_of_classes].


   @raise Invalid_argument if the [data_in_rows] and [classes] do not have the
    same number of data points. *)
let general_eval_and_grad ~lambda k x y =
  let open Bigarray in
  let m = Mat.dim1 x in               (* number of data points *)
  let n = Mat.dim2 x in               (* number of features *)
  let n_y = Array.length y in
  if n_y <> m then
    invalid_arg (Printf.sprintf "different dimensions %d vs %d" n n_y)
  else
    (* Precompute scaling factors and sizees *)
    let nmf  = -1. /. float m in
    let ld2  = lambda /. 2. in
    let vec_size = k * m in
    let ind  = Mat.init_cols k m (fun r c -> if r = y.(c-1) then 1. else 0.) in
    let indv = reshape_1 (genarray_of_array2 ind) vec_size in
    (* Optimization note: Pre-allocating a work space for the generated matrices
       and vectors (pc, dc logd) in the routine below does not seem to improve
       performance all that much (~3-4%) on the Mnist or Default tests in the
       softmax_performance script.

       The next step would be to break down each of the 3 calculations below
       to get their relative timings; I don't have explicit values.
       There are probably some subtle GC interactions here that my rough timing
       scripts are not catching, and this is maybe an area of potential
       improvement.

       Furthermore, this code is intentionally not in a functional style to
       avoid these allocations. Documentation has been added for clarity.
    *)
    let r    = ref 0. in
    let pc   = Mat.make0 k m in
    let dc   = Vec.make0 vec_size in
    let logd = Vec.make0 m in
    begin fun w_v (g_v : Lacaml_float64.vec) ->
      (* unroll w and g into matrices, data is shared so g_v is overwritten. *)
      let w = reshape_2 (genarray_of_array1 w_v) k n in
      let g = reshape_2 (genarray_of_array1 g_v) k n in
      (* compute W * X^T, so that each example is in a column *)
      let p = gemm ~beta:0. ~c:pc w ~transb:`T x in
      (* store the log of sum of exponents of W*X^T_j *)
      for j = 1 to m do
        let c = Mat.col p j in
        let x = Vec.max c in
        let rec loop i a =    (* log-sum-exp trick to avoid overflows. *)
          if i > k then log a +. x
          else loop (i + 1) (a +. exp (c.{i} -. x))
        in
        logd.{j} <- loop 1 0.
      done;
      let cost =
        r := 0.;
        for j = 1 to m do
          let c = Mat.col p j in
          (* Only for the y.(i) element is the indicator true.
             For the numerator avoiding the log (exp WT) -> WT *)
          r := !r +. c.{y.(j-1)} -. logd.{j}
        done;
        nmf *. !r +. ld2 *. Vec.sqr_nrm2 w_v
      in
      let d =
        (* Use the e^(a-b) = e^a / e^b rule to compute
           e^(WX)/sum_k e^(WX_k)) = e^(WX-log(sum_k e^(WX_k)) *)
        let pv = reshape_1 (genarray_of_array2 p) vec_size in
        for i = 1 to k do       (* Subtract the logd from each column element *)
          ignore (Vec.sub ~z:dc ~incz:k ~ofsz:i ~incx:k ~ofsx:i pv logd)
        done;
        ignore (Vec.exp ~y:dc dc);  (* overwrites *)
        reshape_2 (genarray_of_array1 (Vec.sub indv dc)) k m
      in
      ignore (lacpy ~b:g w);
      ignore (gemm ~c:g ~beta:lambda ~alpha:nmf d x);
      cost
    end

let regress ?(tolerance=1e5) ?(lambda=1e-4) x y =
  let k = Array.fold_left max 1 y in
  let n = Mat.dim2 x in
  let w = Vec.random (n * k) in
  let f = general_eval_and_grad ~lambda k x y in
  ignore(Lbfgs.(F.min
                  (*~print:(Every 10*)
                  ~factr:tolerance
                  f
                  w));
  Bigarray.(reshape_2 (genarray_of_array1 w) k n)

let to_probs v =
  Vec.to_list v
  |> List.mapi (fun i p -> (i + 1, p))

let classify_v w x_i =
  let s = Vec.exp (gemv w x_i) in
  scal (1. /. Vec.sum s) s;
  to_probs s

let softmax_tran_rem_col_max_by_column a =
  for j = 1 to Mat.dim2 a do
    let c = Mat.col a j in
    let x = Vec.max c in
    ignore (Vec.add_const (-1. *. x) ~y:c c);
    ignore (Vec.exp ~y:c c);
    let s = 1. /. Vec.sum c in
    scal s c
  done

let classify_m w x =
  let pr = gemm ~transb:`T w x in
  (*Lacaml_util.*)softmax_tran_rem_col_max_by_column pr;
  Mat.to_col_vecs pr
  |> Array.map to_probs
