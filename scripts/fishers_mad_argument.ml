(*
  Inspired by "Revisiting a 90-year-old debate: the advantages of the mean deviation"
  by Stephen Gorard
  http://www.leeds.ac.uk/educol/documents/00003759.htm

  ocamlbuild -use-ocamlfind -pkgs lacaml,ocephes,lbfgs -I src/scripts/ -I src/lib/ fishers_mad_argument.native
*)

open Oml.Util
module D = Oml.Statistics.Descriptive
module S = Oml.Statistics.Sampling

let r = S.normal_std ()
let r_e = S.normal ~mean:0. ~std:9. ()

let mad_norm = sqrt (pi /. 2.)

let sample ?(n=1000) ?error r =
  let data = Array.init n (fun _ -> r ()) in
  let t = match error with | None -> -1 | Some v -> truncate (v *. float n) in
  for i = 0 to t do data.(i) <- r_e () done;
  let s = D.sd data in
  let m = mad_norm *. D.ad ~center:`Mean data in
  s, m

let test ?(m=1000) ?error r =
  let by_sd, by_mad = Array.init m (fun _ -> sample ?error r ) |> Array.unzip in
  D.sd by_sd, D.sd by_mad

let () =
  let s, m = test r in
  Printf.printf "standard: %0.4f\t absolute: %0.4f\t better: %b\n" s m (s < m);
  let error = 0.002 in
  let s_e, m_e = test ~error r in
  Printf.printf "But with %f error\n" error;
  Printf.printf "standard: %0.4f\t absolute: %0.4f\t better: %b\n" s_e m_e (s_e < m_e);
