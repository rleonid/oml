
open Lacaml.D

let r = Sampling.normal_std ()

let gen_lm ?(noise=0.01) r num_pred num_spls =
  let coeff = Array.init num_pred (fun _ -> r ()) in
  let pred  =
    Array.init num_spls (fun _ ->
      Array.init num_pred (fun _ ->
        r ()))
  in
  let resp =
    Matrices.prod_column_vector pred coeff
    |> Array.map (fun i -> i +. (r () *. noise))
  in
  coeff, pred, resp

let coeff, pred, resp = gen_lm r 3 5
let glm = Regression.general_linear_regress ~pad:false ~pred ~resp ()

let looe_manually lambda pred resp =
  let predi = Array.to_list pred |> List.mapi (fun i p -> (i, p)) in
  let respi = Array.to_list resp |> List.mapi (fun i r -> (i, r)) in
  let without i =
    List.filter (fun (j, _) -> j <> i) predi |> Array.of_list |> Array.map snd,
    List.filter (fun (j, _) -> j <> i) respi |> Array.of_list |> Array.map snd
  in
  pred
  |> Array.mapi (fun i p ->
    let p_pred, p_resp = without i in
    let model =
      Regression.general_linear_regress ~lambda:(`Spec lambda)
        ~pad:false ~pred:p_pred ~resp:p_resp ()
    in
    resp.(i) -. Regression.eval_glm model p)

let general_tik ~pred ~resp ~tik =
  let m = Mat.of_array pred in
  let q = Mat.of_array tik in
  let b = Vec.of_array resp in
  Mat.axpy (gemm ~transa:`T m m) q;
  getri q;      (* take inverse with LU decomp*)
  gemv (gemm q ~transb:`T m) b

