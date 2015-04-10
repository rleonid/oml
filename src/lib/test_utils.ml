(* Common functions for writing tests and specifications. *)

open Kaputt.Abbreviations

(* Generators *)
let pos_float = Gen.(filter ((<=) 0.0) float)
let neg_float = Gen.(filter ((>=) 0.0) float)

(* Specifications *)
let is_true x = x
let is_false x = not x
let just_postcond_pred p = Spec.( always ==> p)

