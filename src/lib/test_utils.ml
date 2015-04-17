(* Common functions for writing tests and specifications. *)

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

end

module Gen = FGen (struct let largest_float = max_float end)

(* Specifications *)
module Spec = struct

  let is_true x = x
  let is_false x = not x
  let just_postcond_pred p = Kaputt.Abbreviations.Spec.( always ==> p)
  include Kaputt.Abbreviations.Spec
end

module Test = Kaputt.Abbreviations.Test
module Assert = Kaputt.Abbreviations.Assert
