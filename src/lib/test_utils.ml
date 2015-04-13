(* Common functions for writing tests and specifications. *)

(* Generators *)
module type FloatParameters = sig
  val largest_float : float
end

module FGen (Fp : FloatParameters) = struct
  include Kaputt.Abbreviations.Gen
  let nlarge  = -1.0 *. Fp.largest_float
  let float   = make_float nlarge Fp.largest_float

  let pos_float = filter ((<=) 0.0) float
  let neg_float = filter ((>=) 0.0) float
  let non_zero_float = filter ((<>) 0.0) float

  (* Fixed length *)
  let fl_array n  =
    let msg = "array length " ^ (string_of_int n) in
    array (lift n msg)
  let matrix r c e =
    array r (fl_array c e)

  let array_float n = fl_array n float
  let matrix_float r c = matrix r c float

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
