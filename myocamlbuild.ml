open Ocamlbuild_plugin
open Ocamlbuild_pack
open Printf

let target_with_extension ext =
  List.exists (fun s -> Pathname.get_extension s = ext) !Options.targets

let is_test_target () =
  List.exists (function
    | "oml_test.native"
    | "omlf_test.native" -> true
    | s when (Filename.basename s = "oml_test.native"
           || Filename.basename s = "omlf_test.native") -> true
    | _                      -> false)
    !Options.targets

let is_doc_target () =
  target_with_extension "html"

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

(* For unknown reasons this rule is not firing?
   Or maybe it is insufficient to trigger the generation of a
   full module needed for test. Using this would prevent
   the need for stub code such as functions.ml.
let add_lite_ml_and_mlt_and_depends () =
  let dmlt  = "%.mlt" in
  let doml  = "oml_%.ml" in
  let domll = "oml_lite_%.ml" in
  let pmld  = "%.ml.depends" in
  let pmlj  = "%.mlj" in
  let pmljd = "%.mlj.depends" in
  rule "concat oml_ and oml_lite_ prefixed ml and mlt files, and build dependencies"
    ~insert:`top
    ~deps:[ dmlt; doml; domll ]
    ~prods:[ pmld; pmlj; pmljd ]
    begin fun env _build ->
      let mlt  = env dmlt in
      let oml  = env doml in
      let omll = env domll in
      let mlj  = env pmlj in
      let ml_depends  = env pmld in
      let mlj_depends = env pmljd in
      let ocamldep_tags =
        (Tags.union (tags_of_pathname oml) (tags_of_pathname omll))
        ++ "ocaml" ++ "ocamldep"
      in
      Seq [ Cmd ( S [ A "cat";  P omll;                       Sh ">";  P mlj])
          ; Cmd ( S [ A "cat";  P oml;                        Sh ">>"; P mlj])
          ; Cmd ( S [ A "echo"; A (sprintf "# 0 %S" mlt);     Sh ">>"; P mlj])
          ; Cmd ( S [ A "echo"; A "(*BISECT-IGNORE-BEGIN*)";  Sh ">>"; P mlj])
          ; Cmd ( S [ A "cat";  P mlt;                        Sh ">>"; P mlj])
          ; Cmd ( S [ A "echo"; A "(*BISECT-IGNORE-END*)";    Sh ">>"; P mlj])
          (* Now build the dependencies for the mlj file. *)
          ; Cmd ( S [ A "ocamlfind"; A "ocamldep"; T ocamldep_tags ; A "-ml-synonym"
                    ; Sh "'.mlj'"; A "-modules"; P mlj;       Sh ">";  P mlj_depends])
          (* Fake the ml.depends to be the same as mlj.depends. *)
          ; Cmd ( S [ A "sed"; A "-E"; A "s/mlj/ml/g"; P mlj_depends;
                                                              Sh ">" ; P ml_depends])
          ]
    end *)

let report_dependencies pth =
  let () = printf "here are the %s dependencies\n" pth in
  List.iter (function | `just_try, s  -> printf "just_try: %s\n" s
                      | `mandatory, s -> printf "mandatory: %s\n" s)
    (Ocaml_utils.path_dependencies_of pth)

let add_compile_mlj_to_byte_rule () =
  rule "ocaml: mlj -> cmo"
    ~insert:`top
    ~prods:["%.cmo"; "%.cmi"]
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
              ; Ocaml_utils.ocaml_include_flags mlj
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
        (Tags.union
          (Tags.union (tags_of_pathname ml) (tags_of_pathname mlj))
            (tags_of_pathname cmx))
          ++ "ocaml"
          ++ "implem"
          ++ "native"
          ++ "compile"
      in
      (* Copied from Ocamlbuild source: *)
      Ocaml_compiler.prepare_link cmx cmi ["cmx"; "cmi"] build;
      Cmd ( S [ !Options.ocamlopt; A"-c"; Ocaml_arch.forpack_flags_of_pathname mlj
              ; T tags
              ; Ocaml_utils.ocaml_include_flags mlj
              ; A "-o"; Px cmx (* FIXME ocamlopt bug -o cannot be after the input file *)
              ; A "-impl"; P mlj])
    end

let all_mli_files dir =
  let rec loop acc = function
    | []       -> acc
    | dir :: t ->
        let dirs, files =
          Sys.readdir dir
          |> ArrayLabels.fold_left ~init:([],[]) ~f:(fun (d, m) f ->
              let df = Filename.concat dir f in
              if Sys.is_directory df then
                df :: d, m
              else if Filename.check_suffix df ".mli" then
                d, df :: m
              else
                d, m)
        in
        loop (files @ acc) (dirs @ t)
  in
  loop [] [dir]

let to_mli_assoc =
  List.map (fun s ->
    String.capitalize_ascii (Filename.chop_extension (Filename.basename s)), s)

(* Exclude these two includes from the mli inserting documentation pass. *)
let oml_included_regex, oml_included_length =
  let s = "include module type of Oml\\." in
  Str.regexp s, String.length s - 1

let omlf_included_regex, omlf_included_length =
  let s = "include module type of Omlf\\." in
  Str.regexp s, String.length s - 1

let imto_regex =
  Str.regexp "include module type of \\([A-Z][a-zA-Z_]+\\)"

let search_forward_opt r s p =
  try Some (Str.search_forward r s p)
  with Not_found -> None

let rec include_includes modassoc mli =
  let ic = open_in mli in
  let en = in_channel_length ic in
  let ff = really_input_string ic en in
  close_in ic;
  let rec loop pos acc =
    match search_forward_opt oml_included_regex ff pos with
    | Some pp ->
        let ap = Str.match_end () in
        let bef = String.sub ff pos (pp + oml_included_length - pos) in
        loop ap (bef :: acc)
    | None ->
      begin match search_forward_opt omlf_included_regex ff pos with
      | Some pp ->
          let ap = Str.match_end () in
          let bef = String.sub ff pos (pp + omlf_included_length - pos) in
          loop ap (bef :: acc)
      | None ->
        begin match search_forward_opt imto_regex ff pos with
        | None    -> String.sub ff pos (en - pos) :: acc    (* Safe to assume only 1 per file? *)
        | Some np ->
            let md = Str.matched_group 1 ff in
            let ap = Str.match_end () in
            try
              let file = List.assoc md modassoc in
              Printf.printf "inserting %s for %s ----\n" file md;
              let bef  = String.sub ff pos (np - pos) in
              let ic   =
                if not (Sys.file_exists file) then begin
                  ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.dirname file)));
                  ignore (Sys.command (Printf.sprintf "cp ../%s %s" file file));
                end;
                  include_includes modassoc file;
                  open_in file
              in
              let incf = really_input_string ic (in_channel_length ic) in
              close_in ic;
              loop ap (incf :: bef :: acc)
           with Not_found -> (* Missing module in modassoc *)
              Printf.printf "didn't find %s \n" md;
              let bef  = String.sub ff pos (ap - pos - 1) in
              loop (ap - 1) (bef :: acc)
        end
      end
  in
  match loop 0 [] with
  | [] -> ()
  | lst ->
      let oc = open_out mli in
      List.iter (output_string oc) (List.rev lst);
      close_out oc

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
            let regular_source_dirs =
                  [ "src/onl"
                  ; "src/util"
                  ; "src/unc"
                  ; "src/stats"
                  ; "src/cls"
                  ; "src/rgr"
                  ; "src/uns"
                  ]
            in
            let full_source_dirs =
                  [ "src-full/unc"
                  ; "src-full/stats"
                  ; "src-full/cls"
                  ; "src-full/rgr"
                  ; "src-full/uns"
                  ]
            in

            if is_test_target () then begin
              add_ml_and_mlt_and_depends ();
              (*add_lite_ml_and_mlt_and_depends (); *)
              add_compile_mlj_to_native_rule ();
              add_compile_mlj_to_byte_rule ();
              Pathname.define_context "test" regular_source_dirs;
              Pathname.define_context "test" full_source_dirs;
              List.iter (fun dir -> Pathname.define_context dir [ "test"])
                regular_source_dirs;
              List.iter (fun dir -> Pathname.define_context dir [ "test"])
                full_source_dirs
            end;

            (* To build without interfaces
            rule "ocaml-override: ml -> cmo & cmi"
              ~insert:`top
              ~prods:["%.cmo"; "%.cmi"]
              ~deps:["%.ml"; "%.ml.depends"]
              ~doc:"This rule disables mli files."
              (Ocaml_compiler.byte_compile_ocaml_implem "%.ml" "%.cmo");
            *)

            if is_doc_target () then begin
              Pathname.define_context "src" regular_source_dirs;
              Pathname.define_context "src-full" full_source_dirs;

              (* Since ocamldoc doesn't work very well with
                 "include module type of ___" directives, we'll preprocess the
                 mli files to manually insert the relevant signature.
                 1. Get all the mli's.
                 2. Before compiling them, scan for "include m... sig"
                 3. If we find this pattern and we have a signature file.
                    3a. Recurse!
                 4. Manually insert it into the mli.
              *)
              let mla =
                (all_mli_files "src"
                @ all_mli_files "src-full")
                |> to_mli_assoc
                |> fun l ->
                    ("Oml_classification_input_interfaces", "src/cls/oml_classification_input_interfaces.ml") ::
                    l
              in
              Printf.printf "We have these files:\n%!";
              List.iter (fun (m,f) -> Printf.printf "%s\t\t%s\n%!" m f) mla;

              rule "For documentation ocaml: mli -> cmi"
                ~insert:`top
                ~deps:[ "%.mli"; "%.mli.depends" ]
                ~prods:[ "%.cmi" ]
                begin fun env build ->
                  let mli = env "%.mli" in
                  include_includes mla mli;
                  Ocaml_compiler.compile_ocaml_interf "%.mli" "%.cmi" env build
                end;
            end
          end
  in
  dispatch additional_rules
