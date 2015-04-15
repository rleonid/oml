
open Lacaml.D

let svd m =
  let open Lacaml.D in
  let s, u, vt = gesvd (Mat.of_array m) in
  let s   = Vec.to_array s
  and u   = Mat.to_array u
  and vt  = Mat.to_array vt in
  u, s, vt

(* TODO: expose the dimensionality reduction. *)
let solve_linear a b =
  let u, s, vt = svd a in
  let s_inv = Array.map (function 0.0 -> 0.0 | x -> 1.0 /. x) s in
  let s_mat = Matrices.diagonal s_inv in
  let b_mat = Array.init (Array.length b) (fun c -> [| b.(c) |]) in
  (* should I have kept these in Lacaml? *)
  Matrices.(prod (transpose u) b_mat)
  |> Matrices.prod s_mat
  |> Matrices.prod vt
