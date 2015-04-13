(* Common functions for writing tests and specifications. *)

(* Generators *)
module Gen = struct
  module KG = Kaputt.Generator
  let pos_float = KG.(filter ((<=) 0.0) float)
  let neg_float = KG.(filter ((>=) 0.0) float)
  let array_fl n  = 
    let msg = "array length " ^ (string_of_int n) in
    KG.(array (lift n msg))
  let matrix r c e = KG.(array r (array_fl c e))
  include Kaputt.Abbreviations.Gen
end

(* Specifications *)
module Spec = struct

  let is_true x = x
  let is_false x = not x
  let just_postcond_pred p = Kaputt.Abbreviations.Spec.( always ==> p)
  include Kaputt.Abbreviations.Spec
end

module Test = Kaputt.Abbreviations.Test
module Assert = Kaputt.Abbreviations.Assert
