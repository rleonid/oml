open Lacaml_D

(* Classes (ie values in y) in y = {1,2 ... k}
   General softmax method.
*)
let general_eval_and_grad ~newmethod ~lambda k x y =
  let open Bigarray in
  let m = Mat.dim1 x in         (* number of data points *)
  let n = Mat.dim2 x in         (* number of features *)
  let n_y = Array.length y in
  if n_y <> m then
    invalid_arg (Printf.sprintf "different dimensions %d vs %d" n n_y)
  else
    let nmf = -1. /. float m in
    let ld2 = lambda /. 2. in
    let ind = Mat.init_cols k m (fun r c -> if r = y.(c-1) then 1. else 0.) in
    if newmethod then
      begin fun w_c (g_c : Lacaml_float64.vec) ->
        (* unroll w and g *)
        let w = reshape_2 (genarray_of_array1 w_c) k n in
        let g = reshape_2 (genarray_of_array1 g_c) k n in
        let p = gemm w ~transb:`T x in
        let logd =
          (* TODO: pre-allocate work space *)
          Vec.init m (fun j ->
            let c = Mat.col p j in
            let x = Vec.max c in
            Vec.add_const (-1. *. x) c      (* log-sum-exp 'trick' *)
            |> Vec.exp
            |> Vec.sum
            |> log
            |> (+.) x)
        in
        let s =
          Array.fold_left (fun (s,j) i ->
            let c = Mat.col p j in
            s +. c.{i} -. logd.{j}, j + 1)
            (0.,1) y
            |> fst
            |> fun s -> nmf *. s +. ld2 *. Vec.sqr_nrm2 w_c
        in
        let d =
          Array.init m (fun j ->
              let j = j + 1 in
              let c = Mat.col p j in
              let f = Vec.add_const (-1. *. logd.{j}) c |> Vec.exp in
              Vec.sub (Mat.col ind j) f)
          |> Mat.of_col_vecs
        in
        ignore (lacpy ~b:g w);
        ignore (gemm ~c:g ~beta:lambda ~alpha:nmf d x);
        s
    end else
      begin fun w_c (g_c : Lacaml_float64.vec) ->
        (* unroll w and g *)
        let w = reshape_2 (genarray_of_array1 w_c) k n in
        let g = reshape_2 (genarray_of_array1 g_c) k n in
        let p = gemm w ~transb:`T x in
        Lacaml_util.remove_column_max p;
        Lacaml_util.softmax_tran_by_column p;
        let s =
          (* one could do this entirely in "Fortran" by
            1. convert p to a vector -> p_v
            2. Vec.log of p_v
            3. convert ind to a vector -> ind_v
            4. Vec.add p_v ind_v *)
          Array.fold_left (fun (s, j) i ->
              let v = (Mat.col p j).{i} in
              s +. log (max min_float v), j + 1)
            (0.,1) y
          |> fst
          |> fun s -> nmf *. s +. ld2 *. Vec.sqr_nrm2 w_c
        in
        let d =
          Array.init m (fun j ->
              let j = j + 1 in
              Vec.sub (Mat.col ind j) (Mat.col p j))
          |> Mat.of_col_vecs
        in
        ignore (lacpy ~b:g w);
        ignore (gemm ~c:g ~beta:lambda ~alpha:nmf d x);
        s
    end

let regress ?p ?(lambda=1e-4) ?(newmethod=true) x y =
  let k = Array.fold_left max 1 y in
  let n = Mat.dim2 x in
  let w = Vec.random (n * k) in
  let () = match p with | None -> () | Some s -> scal s w in
  ignore(Lbfgs.(F.min (*~print:(Every 10*)
                    (general_eval_and_grad ~newmethod ~lambda k x y) w));
  Bigarray.(reshape_2 (genarray_of_array1 w) k n)

let to_probs v =
  Vec.to_list v
  |> List.mapi (fun i p -> (i + 1, p))

let classify_v w x_i =
  let s = Vec.exp (gemv w x_i) in
  scal (1. /. Vec.sum s) s;
  to_probs s

let classify_m w x =
  let pr = gemm ~transb:`T w x in
  Lacaml_util.softmax_tran_rem_col_max_by_column pr;
  Mat.to_col_vecs pr
  |> Array.map to_probs
