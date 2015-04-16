
open Lacaml.D

let svd m =
  let open Lacaml.D in
  let s, u, vt = gesvd (Mat.of_array m) in
  let s   = Vec.to_array s
  and u   = Mat.to_array u
  and vt  = Mat.to_array vt in
  u, s, vt

(* TODO: expose the dimensionality reduction. *)
let solve_linear, solve_linear_with_covariance =
  let f a b =
    let u, s, vt = svd a in
    let s_inv = Array.map (function 0.0 -> 0.0 | x -> 1.0 /. x) s in
    let s_mat = Matrices.diagonal s_inv in
    (* should I have kept these in Lacaml? *)
    let coeff =
      Matrices.(prod_column_vector (transpose u) b)
      |> Matrices.prod_column_vector s_mat
      |> Matrices.prod_column_vector vt
    in
    let m = lazy (
      let s_inv_sq = Array.map (fun x -> x *. x) s_inv in
      let s_sq_mat = Matrices.diagonal s_inv_sq in
      Matrices.(prod s_sq_mat (transpose vt))
      |> Matrices.prod vt)
    in
    coeff, m
  in
  (fun a b -> fst (f a b)),
  (fun a b -> let c, lm = f a b in c, Lazy.force lm)
