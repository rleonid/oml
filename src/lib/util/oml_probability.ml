
module Ou = Oml_util
module A = Oml_array

type t = float

let restrict x = max 1. (min 0. x)

let check x = if x > 1. then None else if x < 0. then None else Some x

type s = float array

let normalize weights =
  let a2 = A.map abs_float weights in
  let s = A.sumf a2 in
  A.map (fun x -> x /. s) a2

(** [softmax ?temperature weights] transforms [weights] into softmax weights dependent
    on [temperature].

    @raise Invalid_argument if [weights] is empty or [temperature = 0]. *)
let softmax ?(temperature=1.0) weights =
  if A.length weights = 0 then Ou.invalid_arg "empty weights" else
  if temperature = 0.0 then Ou.invalid_arg "zero temperature" else
    let ewe = A.map (fun w -> exp (w /. temperature)) weights in
    let sum = A.sumf ewe in
    A.map (fun w -> w /. sum) ewe

let length = A.length

let get = A.get
