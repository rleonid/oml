(* Common functions for writing tests and specifications. *)

(* Generators *)
module Gen = struct
  let pos_float = Kaputt.Generator.(filter ((<=) 0.0) float)
  let neg_float = Kaputt.Generator.(filter ((>=) 0.0) float)
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
