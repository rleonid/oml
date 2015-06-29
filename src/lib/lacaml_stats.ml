
open Lacaml.D

(* column mean. *)
let col_mean n c = Vec.sum c /. n

(* sum of squares diff of col *)
let sum_sq_dm c m = Vec.ssqr (Vec.add_const (-.m) c)

(* column standard deviation *)
let col_std c m n = sqrt (sum_sq_dm c m /. n)

let normalize ?(demean=true) ?(scale=true) ?(unbiased=true) m =
  let r = Mat.dim1 m in
  let n = float r in
  let f col =
    let m = if demean then col_mean n col else 0.0 in
    let s =
      if scale then
        col_std col m (if unbiased then n -. 1.0 else n)
        else 1.0
    in
    let dm  = Vec.make r (-.m/.s) in
    axpy ~alpha:(1.0/.s) col dm;
    ((m,s),dm)
  in
  let adj_cols_lst = Mat.fold_cols (fun acc col -> (f col)::acc) [] m |> List.rev in
  let adj = List.map fst adj_cols_lst in
  let mmm =
    List.map snd adj_cols_lst
    |> Array.of_list
    |> Mat.of_col_vecs
  in
  adj, mmm
