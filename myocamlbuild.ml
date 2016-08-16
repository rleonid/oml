open Ocamlbuild_plugin
open Ocamlbuild_pack
open Printf

let target_with_extension ext =
  List.exists (fun s -> Pathname.get_extension s = ext) !Options.targets

let add_test_target_rule () =
  rule "Create a test target."
    ~prod:"%.test"
    ~dep:"%.native"
    begin fun env _build ->
      let test = env "%.test" and native = env "%.native" in
      Seq [ mv native test
          ; Cmd ( S [ A "ln"
                    ; A "-sf"
                    ; P (!Options.build_dir/test)
                    ; A Pathname.parent_dir_name])
          ]
    end

let add_ml_and_mlt_and_depends () =
  rule "concat ml and mlt files, and build dependencies"
    ~insert:`top
    ~deps:["%.mlt"; "%.ml"]
    ~prods:["%.ml.depends"; "%.mlj"; "%.mlj.depends"]
    begin fun env _build ->
      let ml  = env "%.ml" in
      let mlt = env "%.mlt" in
      let mlj = env "%.mlj" in
      let ml_depends  = env "%.ml.depends" in
      let mlj_depends = env "%.mlj.depends" in
      let ocamldep_tags = tags_of_pathname ml ++ "ocaml" ++ "ocamldep" in
      Seq [ Cmd ( S [ A "cat" ; P ml;                         Sh ">"; P mlj])
          ; Cmd ( S [ A "echo"; A (sprintf "# 0 %S" mlt);     Sh ">>"; P mlj])
          ; Cmd ( S [ A "echo"; A "(*BISECT-IGNORE-BEGIN*)";  Sh ">>"; P mlj])
          ; Cmd ( S [ A "cat";  P mlt;                        Sh ">>"; P mlj])
          ; Cmd ( S [ A "echo"; A "(*BISECT-IGNORE-END*)";    Sh ">>"; P mlj])
          (* Now build the dependencies for the mlj file. *)
          ; Cmd ( S [ A "ocamlfind"; A "ocamldep"; T ocamldep_tags ; A "-ml-synonym"
                    ; Sh "'.mlj'"; A "-modules"; P mlj;       Sh ">"; P mlj_depends])
          (* Fake the ml.depends to be the same as mlj.depends. *)
          ; Cmd ( S [ A "sed"; A "-E"; A "s/mlj/ml/g"; P mlj_depends;
                                                              Sh ">" ; P ml_depends])
          ]
    end

let report_dependencies pth =
  let () = printf "here are the %s dependencies\n" pth in
  List.iter (function | `just_try, s  -> printf "just_try: %s\n" s
                      | `mandatory, s -> printf "mandatory: %s\n" s)
    (Ocaml_utils.path_dependencies_of pth)

let add_compile_mlj_to_byte_rule () =
  rule "ocaml: mlj -> cmo"
    ~insert:`top
    ~prods:["%.cmo"]
    ~deps:["%.mlj"; "%.mlj.depends"]
    begin fun env build ->
      let ml  = env "%.ml" in
      let mlj = env "%.mlj" in
      let cmo = env "%.cmo" in
      let tags =
        (Tags.union (tags_of_pathname ml) (tags_of_pathname mlj))
          ++ "ocaml"
          ++ "implem"
          ++ "byte"
          ++ "compile"
      in
      (* Copied from Ocamlbuild source: *)
      Ocaml_compiler.prepare_compile build mlj;
      Cmd ( S [ !Options.ocamlc; A"-c"; Ocaml_arch.forpack_flags_of_pathname mlj
              ; T tags
              ; (*!Options.ocaml_ppflags tags
              ; *) Ocaml_utils.ocaml_include_flags mlj
              ; A "-o"; Px cmo
              ; A "-impl"; P mlj])
    end

let add_compile_mlj_to_native_rule () =
  rule "ocaml: mlj -> cmx & o"
    ~insert:`top
    ~prods:["%.cmx"; "%" -.- !Options.ext_obj ]
    ~deps:["%.mlj"; "%.mlj.depends"]
    begin fun env build ->
      let ml  = env "%.ml" in
      let mlj = env "%.mlj" in
      let cmx = env "%.cmx" in
      let cmi = Pathname.update_extensions "cmi" cmx in
      let tags =
        (Tags.union (tags_of_pathname ml) (tags_of_pathname mlj))
          ++ "ocaml"
          ++ "implem"
          ++ "native"
          ++ "compile"
      in
      (* Copied from Ocamlbuild source: *)
      Ocaml_compiler.prepare_link cmx cmi ["cmx"; "cmi"] build;
      Cmd ( S [ !Options.ocamlopt; A"-c"; Ocaml_arch.forpack_flags_of_pathname mlj
              ; T tags
              ; (*!Options.ocaml_ppflags tags
              ; *) Ocaml_utils.ocaml_include_flags mlj
              ; A "-o"; Px cmx (* FIXME ocamlopt bug -o cannot be after the input file *)
              ; A "-impl"; P mlj])
    end

let () =
  let additional_rules =
    function
      | Before_hygiene  -> ()
      | After_hygiene   -> ()
      | Before_options  -> ()
      | After_options   -> ()
      | Before_rules    -> ()
      | After_rules     ->
          begin
            Pathname.define_context "src/lib/unc"   ["src/lib/util"; "src/lib/unc"];
            Pathname.define_context "src/lib/stats" ["src/lib/util"; "src/lib/unc"; "src/lib/stats"];
            Pathname.define_context "src/lib/cls"   ["src/lib/util"; "src/lib/unc"; "src/lib/stats"; "src/lib/cls"];
            Pathname.define_context "src/lib/rgr"   ["src/lib/util"; "src/lib/unc"; "src/lib/stats"; "src/lib/rgr"];
            Pathname.define_context "src/lib/uns"   ["src/lib/util"; "src/lib/unc"; "src/lib/uns"];

            if target_with_extension "test" then begin
              add_ml_and_mlt_and_depends ();
              add_compile_mlj_to_native_rule ();
              add_compile_mlj_to_byte_rule ();
            end;

            add_test_target_rule ();

            (* To build without interfaces
            rule "ocaml-override: ml -> cmo & cmi"
              ~insert:`top
              ~prods:["%.cmo"; "%.cmi"]
              ~deps:["%.ml"; "%.ml.depends"]
              ~doc:"This rule disables mli files."
              (Ocaml_compiler.byte_compile_ocaml_implem "%.ml" "%.cmo") ;
            *)
            (* For documentation. *)
            if target_with_extension "html" then begin
              (* Insert Oml_array.mli into the
                'include (module type of Oml_array)'
                so taht we can have the signature for documentation. *)
              let from_file = "src/lib/util/util.mli" in
              let to_file   = "_build/src/lib/util/util.mli" in
              let perl_mat  = "include \\(module type of Oml_array\\)" in
              let command   =
                sprintf
                  "perl -pe 's/%s/`cat src\\/lib\\/util\\/oml_array.mli`/ge' %s > %s"
                    perl_mat from_file to_file
              in
              ignore (Sys.command "mkdir -p _build/src/lib/util");
              printf "%s\n" command;
              ignore (Sys.command command);
              rule "Create mli from mlpack."
                ~prod:"%.mli"
                ~deps:["%.mlpack"; "%.mlipack"]
                begin fun env _build ->
                  let pck = env "%.mlpack" in
                  let pcki = env "%.mlipack" in
                  let dir = Pathname.pwd / Pathname.dirname pck in
                  let mli = !Options.build_dir / env "%.mli" in
                  string_list_of_file pck
                  |> List.map (fun mdl ->
                    let fname = dir / String.lowercase_ascii (mdl ^ ".mli") in
                    let mdlfl = env fname in
                    if Pathname.exists mdlfl then begin
                      [ Sh (sprintf "echo module %s : sig >>" mdl)
                      ; P mli
                      ; Sh ";"
                      ; Sh (sprintf "cat %s >>" mdlfl)
                      ; P mli
                      ; Sh ";"
                      ; Sh "echo end >>"
                      ; P mli
                      ; Sh ";"
                      ]
                    end else
                      [])
                  |> List.concat
                  |> fun lst ->
                      let nlst = Sh (sprintf "cat %s >>" pcki)
                                 :: P mli
                                 :: Sh ";"
                                 :: lst
                      in
                      Cmd (S nlst)
                end
            end
          end
  in
  dispatch additional_rules
