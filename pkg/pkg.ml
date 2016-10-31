#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let lacaml = Conf.with_pkg "lacaml"
let lbfgs = Conf.with_pkg "lbfgs"
let ocephes = Conf.with_pkg "ocephes"
let kaputt = Conf.with_pkg "kaputt"
let bisect_ppx = Conf.with_pkg "bisect_ppx"
let coverage = Conf.with_pkg ~default:false "coverage"
let documentation = Conf.with_pkg ~default:false "documentation"

let full c =
  let lacaml = Conf.value c lacaml in
  let lbfgs = Conf.value c lbfgs in
  let ocephes = Conf.value c ocephes in
  lacaml && lbfgs && ocephes

let packages ps =
  List.fold_left (fun acc p -> p :: "-package" :: acc) [] ps
  |> List.rev
  |> Cmd.of_list

let () =
  let build =
    let cmd c os fpaths =
      let full     = full c in
      let coverage = Conf.value c coverage in
      let is_test  = Conf.pkg_name c = "omltest" in
      let scmd =
        Cmd.(Pkg.build_cmd c os
          %% (of_list ["-plugin-tag"; "package(str)"])       (* Str in myocamlbuild.ml *)
          %% on full      (packages [ "ocephes"; "lacaml"; "lbfgs"])
          %% on coverage  (packages [ "bisect_ppx"])
          %% on is_test   (packages [ "dsfo" ; "kaputt" ])
          %% of_list fpaths)
      in
      OS.Cmd.run scmd
    in
    Pkg.build ~cmd ()
  in
  Pkg.describe ~build "oml" @@ fun c ->
    let full = full c in
    match Conf.pkg_name c with
    | "oml" ->
        Ok  [ Pkg.mllib ~api:["Oml"] "src/oml.mllib"
            ; Pkg.mllib ~cond:full ~api:["Oml_full"] "src-full/oml_full.mllib"
            ]
    | "omltest" ->
        Ok  [ Pkg.test (if full then "test/omlf_test" else "test/oml_test") ]
    | other ->
      R.error_msgf "Unrecognized package name: %s" other
