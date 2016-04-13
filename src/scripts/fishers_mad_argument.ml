
(*
ocamlbuild -use-ocamlfind -pkgs lacaml,ocephes,lbfgs -I src/scripts/ -I src/lib/ fishers_mad_argument.native
*)

open Oml.Util
module D = Oml.Statistics.Descriptive
module S = Oml.Statistics.Sampling

let r = S.normal_std ()

let mad_norm = sqrt (pi /. 2.)

let sample ?(n=1000) r =
  let data = Array.init n (fun _ -> r ()) in
  D.sd data, mad_norm *. D.ad ~center:`Mean data

let test ?(m=1000) r =
  let by_sd, by_mad = Array.init m (fun _ -> sample r ) |> Array.unzip in
  D.sd by_sd, D.sd by_mad

let () =
  let s, m = test r in
  Printf.printf "standard: %0.4f\t absolute: %0.4f\t better: %b\n"
    s m (s < m)
