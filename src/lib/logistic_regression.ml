open Lacaml_D

(* Code modified from
  http://math.umons.ac.be/anum/fr/software/OCaml/Logistic_Regression/ *)

(* [logistic_grad_n_eval] returns the value of the function to maximize
  and store its gradient in [g]. *)
let logistic_grad_n_eval ~lambda x y =
  let nm1 = Array.length x - 1 in
  (fun w g ->
    let s = ref 0. in
    ignore(copy ~y:g w);                   (* g ← w *)
    scal (-. lambda) g;                    (* g = -λ w *)
    for i = 0 to nm1 do
      let yi = y.(i) in
      let e  = exp(-. yi *. dot w x.(i)) in
      s := !s +. log1p e;
      axpy x.(i) ~alpha:(yi *. e /. (1. +. e)) g;
    done;
    -. !s -. 0.5 *. lambda *. dot w w)

(* Perform the regression. *)
let log_reg ?(lambda=0.1) x y =
  let w = Vec.make0 (Vec.dim x.(0)) in
  ignore(Lbfgs.F.max (*~print:(Lbfgs.Every 10) *)
          (logistic_grad_n_eval ~lambda x y) w);
  w
