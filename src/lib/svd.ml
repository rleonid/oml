
open Lacaml.D

let svd m =
  let open Lacaml.D in
  let s, u, vt = gesvd (Mat.of_array m) in
  let s   = Vec.to_array s
  and u   = Mat.to_array u
  and vt  = Mat.to_array vt in
  u, s, vt
