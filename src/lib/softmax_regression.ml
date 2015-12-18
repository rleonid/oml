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
    let r   = ref 0. in
    let vs  = k * m in
    if newmethod then
      begin
        fun w_c (g_c : Lacaml_float64.vec) ->
        (* unroll w and g *)
        let w = reshape_2 (genarray_of_array1 w_c) k n in
        let g = reshape_2 (genarray_of_array1 g_c) k n in
        let p = gemm w ~transb:`T x in
        let logd =
          (* Weirdly, pre-allocating a work space for the 'add_const' and 'exp'
             below doesn't improve performance. *)
          Vec.init m (fun j ->
            let c = Mat.col p j in
            let x = Vec.max c in
            let rec loop i a =
              if i > k then log a +. x
              else loop (i + 1) (a +. exp (c.{i} -. x))
            in
            loop 1 0.)
        in
        let s =
          r := 0.;
          for j = 1 to m do
            let c = Mat.col p j in
            (* only for the y.(i) element is the indicator true *)
            r := !r +. c.{y.(j-1)} -. logd.{j}
          done;
          nmf *. !r +. ld2 *. Vec.sqr_nrm2 w_c
        in
        let d =
          let pv = reshape_1 (genarray_of_array2 p) vs in
          let dv = Vec.make0 vs in
          for i = 1 to n do
            (* Subtract the logd from each column element *)
            ignore (Vec.sub ~z:dv ~incz:n ~ofsz:i ~incx:n ~ofsx:i pv logd)
          done;
          ignore (Vec.exp ~y:dv dv);  (* overwrites *)
          let indv = reshape_1 (genarray_of_array2 ind) vs in
          reshape_2 (genarray_of_array1 (Vec.sub indv dv)) k m;
        in
        ignore (lacpy ~b:g w);
        ignore (gemm ~c:g ~beta:lambda ~alpha:nmf d x);
        s
    end else
      begin
        fun w_c (g_c : Lacaml_float64.vec) ->
        (* unroll w and g *)
        let w = reshape_2 (genarray_of_array1 w_c) k n in
        let g = reshape_2 (genarray_of_array1 g_c) k n in
        let p = gemm w ~transb:`T x in
        let logd =
          (* Weirdly, pre-allocating a work space for the 'add_const' and 'exp'
             below doesn't improve performance. *)
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
          r := 0.;
          for j = 1 to m do
            let c = Mat.col p j in
            (* only for the y.(i) element is the indicator true *)
            r := !r +. c.{y.(j-1)} -. logd.{j}
          done;
          nmf *. !r +. ld2 *. Vec.sqr_nrm2 w_c
        in
        let d =
          let pv = reshape_1 (genarray_of_array2 p) vs in
          let dv = Vec.make0 vs in
          for i = 1 to n do
            (* Subtract the logd from each column element *)
            ignore (Vec.sub ~z:dv ~incz:n ~ofsz:i ~incx:n ~ofsx:i pv logd)
          done;
          ignore (Vec.exp ~y:dv dv);  (* overwrites *)
          let indv = reshape_1 (genarray_of_array2 ind) vs in
          reshape_2 (genarray_of_array1 (Vec.sub indv dv)) k m;
        in
        ignore (lacpy ~b:g w);
        ignore (gemm ~c:g ~beta:lambda ~alpha:nmf d x);
        s
    end

let regress ?(tolerance=1e5) ?(lambda=1e-4) ?(newmethod=true) x y =
  let k = Array.fold_left max 1 y in
  let n = Mat.dim2 x in
  let w = Vec.random (n * k) in
  let f = general_eval_and_grad ~newmethod ~lambda k x y in
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

let classify_m w x =
  let pr = gemm ~transb:`T w x in
  Lacaml_util.softmax_tran_rem_col_max_by_column pr;
  Mat.to_col_vecs pr
  |> Array.map to_probs
