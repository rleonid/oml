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

let () =
  let build =
    let cmd c os fpaths =
      let scmd =
        Cmd.(Pkg.build_cmd c os
          %% (of_list ["-plugin-tag"; "package(str)"])       (* Cause of Str in myocamlbuild.ml *)
          %% on (Conf.value c coverage) (of_list ["-package"; "bisect_ppx"])
          %% on (Conf.pkg_name c = "omltest") (of_list [ "-package"; "dsfo"
                                                       ; "-package"; "kaputt"
                                                       ])
          %% of_list fpaths)
      in
      OS.Cmd.run scmd
    in
    Pkg.build ~cmd ()
  in
  Pkg.describe ~build "oml" @@ fun c ->
    let lacaml = Conf.value c lacaml in
    let lbfgs = Conf.value c lbfgs in
    let ocephes = Conf.value c ocephes in
    let full = lacaml && lbfgs && ocephes in
    match Conf.pkg_name c with
    | "oml" ->
        Ok  [ Pkg.mllib ~api:["Oml"] "src/oml.mllib"
            ; Pkg.mllib ~cond:full ~api:["Oml_full"] "src-full/oml_full.mllib"
            ]
    | "omltest" ->
        let kaputt = Conf.value c kaputt in
        (* TODO: Add dsfo! *)
        let full = full && kaputt in
        Ok  [ Pkg.test (if full then "test/omlf_test" else "test/oml_test") ]
    | other ->
      R.error_msgf "Unrecognized package name: %s" other
