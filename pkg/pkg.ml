#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let lacaml = Conf.with_pkg "lacaml"
let lbfgs = Conf.with_pkg "lbfgs"
let ocephes = Conf.with_pkg "ocephes"

let kaputt = Conf.with_pkg "kaputt"

let () =
  Pkg.describe "oml" @@ fun c ->
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
      let full = full && kaputt in
      Ok  [ Pkg.mllib ~api:["Oml"] "src/oml.mllib"
          ; Pkg.mllib ~cond:full ~api:["Oml_full"] "src-full/oml_full.mllib"
          ; Pkg.test "test/oml_test"
          ]
  | other ->
    R.error_msgf "Unrecognized package name: %s" other
