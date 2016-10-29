#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let lacaml = Conf.with_pkg ~default:false "lacaml"
let lbfgs = Conf.with_pkg ~default:false "lbfgs"
let ocephes = Conf.with_pkg ~default:false "ocephes"
let kaputt = Conf.with_pkg "kaputt"
let bisect_ppx = Conf.with_pkg "bisect_ppx"
let coverage = Conf.with_pkg ~default:false "coverage"

let () =
  let build =
    let cmd c os fpaths =
      let scmd =
        if Conf.value c coverage
        then Cmd.((Pkg.build_cmd c os) % "-package" % "bisect_ppx" %% of_list fpaths)
        else Cmd.(Pkg.build_cmd c os %% of_list fpaths)
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
        Ok  [ Pkg.mllib ~api:["Oml"] "src/oml.mllib"
            ; Pkg.mllib ~cond:full ~api:["Oml_full"] "src-full/oml_full.mllib"
            ; Pkg.test "test/oml_test"
            ]
    | other ->
      R.error_msgf "Unrecognized package name: %s" other
