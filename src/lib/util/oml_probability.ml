
module Ou = Oml_util
module A = Oml_array

type s = float array

let normalize weights =
  let a2 = A.map abs_float weights in
  let s = A.sumf a2 in
  A.map (fun x -> x /. s) a2

let softmax ?(temperature=1.0) weights =
  if A.length weights = 0 then Ou.invalidArg "empty weights" else
  if temperature = 0.0 then Ou.invalidArg "zero temperature" else
    let ewe = A.map (fun w -> exp (w /. temperature)) weights in
    let sum = A.sumf ewe in
    A.map (fun w -> w /. sum) ewe

let length = A.length

let get = A.get
