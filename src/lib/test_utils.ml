(* Common functions for writing tests and specifications. *)

open Printf

(* Generators *)
module type FloatParameters = sig
  val largest_float : float
end

module FGen (Fp : FloatParameters) = struct
  include Kaputt.Abbreviations.Gen
  let nlarge  = -1.0 *. Fp.largest_float
  let float   = (fun r ->
      let s = Random.State.bool r in
      let x = Random.State.float r Fp.largest_float in
      if s then x else -.x),
      string_of_float

  let pos_float = filter ((<=) 0.0) float
  let neg_float = filter ((>=) 0.0) float
  let non_zero_float = filter ((<>) 0.0) float

  (* Fixed length *)
  let fixed_length_array n  =
    let msg = "array length " ^ (string_of_int n) in
    array (lift n msg)

  let fixed_length_matrix r c e =
    let row_msg = "row length " ^ string_of_int r
    and col_msg = "col length " ^ string_of_int c in
    array (lift r row_msg) (array (lift c col_msg) e)

  let matrix (r, _) (c, _) (e, es) =
    (fun random ->
      let rows = r random and columns = c random in
      Array.init rows (fun _ ->
        Array.init columns (fun _ ->
          e random))),
    (fun m ->
      m
      |> Array.map (Kaputt.Utils.make_string_of_array es)
      |> Array.to_list
      |> String.concat "\n")

  let array_float n = fixed_length_array n float
  let matrix_float r c = fixed_length_matrix r c float
  let sq_float_matrix s = matrix_float s s

  let print_float_array m =
    m
    |> Array.map (Kaputt.Utils.make_string_of_array (snd float))
    |> Array.to_list
    |> String.concat "\n"

  let general_model_array ~max_predictors ~max_samples =
    zip2 (make_int 2 max_predictors)
         (make_int 2 max_samples)
    (* +1 is for how we build samples models below. *)
    |> transform (fun (p, s) -> 1 + max p s, min p s)
    |> fun (rcg, _) ->
        (* TODO: refactor this into a bind. *)
        (fun random ->
          let (rows, columns) = rcg random in
            Array.init rows (fun _ ->
              Array.init columns (fun _ ->
                (fst float) random))),
        print_float_array

  let general_model ~max_predictors ~max_samples =
    general_model_array max_predictors max_samples
    |> map1 (fun m ->
      let data = Array.sub m 1 (Array.length m - 1) in
      let coef = m.(0) in
      let prod_column_vector m v =
        let row_l = Array.length m in
        let col_l = Array.length m.(0) in
        let n = col_l - 1 in
        let s = Pervasives.ref 0.0 in
        Array.init row_l (fun r ->
          s := 0.0;
          for i = 0 to n do
            s := !s +. v.(i) *. m.(r).(i)
          done;
          !s)
      in
      let resp = prod_column_vector data coef in
      (data, coef, resp))
      (fun (pred, coef, resp) ->
        sprintf "predictors: %s\n coefficients: %s\n response: %s\n"
          (print_float_array pred)
          (Kaputt.Utils.make_string_of_array (snd float) coef)
          (Kaputt.Utils.make_string_of_array (snd float) resp))

end

module Gen = FGen (struct let largest_float = max_float end)

(* Specifications *)
module Spec = struct

  let is_true x = x
  let is_false x = not x
  let just_postcond_pred p = Kaputt.Abbreviations.Spec.( always ==> p)
  let is_invalid_arg = function | Invalid_argument _ -> true | _ -> false
  include Kaputt.Abbreviations.Spec
end

module Test = Kaputt.Abbreviations.Test
module Assert = struct
  include Kaputt.Abbreviations.Assert
  let equalf = equal ~prn:(sprintf "%0.4f")
end

