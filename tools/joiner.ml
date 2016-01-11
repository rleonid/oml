(*
 * This utility concats the test file (.mlt) at the end of the regular source
 * (.ml) file, modifying the source in the meantime. This should be used in
 * a copied build directory (such as if you're using OCamlbuild).
 *
 * We need to modify the files in place so that Bisect (used afterwards)
 * can easily detect which file (specifically filename) it is instrumenting,
 * as opposed to a random generated tempfile by camlp4.
 *
 * It is adapted from kaputt_pp.ml found in Kaputt, which performs a similar
 * processing. The associated license is included below.
 *
 * This file is part of Kaputt.
 * Copyright (C) 2008-2012 Xavier Clerc.
 *
 * Kaputt is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Kaputt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

let call args =
  let quote s =
    if (s <> "") && (s.[0] = '-') then
      Filename.quote s
    else
      s in
  let len = Array.length args in
  let buff = Buffer.create 128 in
  for i = 0 to pred len do
    Buffer.add_string buff (quote args.(i));
    Buffer.add_char buff ' ';
  done;
  (*Printf.eprintf "Calling: %s\n" (Buffer.contents buff); *)
  let code = Sys.command (Buffer.contents buff) in
  ignore (exit code)

let copy from_chan to_chan =
  try
    while true do
      output_string to_chan (input_line from_chan);
      output_char to_chan '\n';
    done
  with _ -> ()

let joined_header ~source_file ~test_file =
  Printf.sprintf "(* concating files for glory %S %S *)"
    source_file test_file

let starts_with prefix str =
  let lp = String.length prefix in
  (str <> "") &&
  String.length str >= lp &&
  String.sub str 0 lp = prefix

(* Argument Format:
  joiner.exe camlp4o other arguments source_file.ml

  If source_file.mlt exists and source_file.ml doesn't
  have our guard, concat add the mlt to end of ml
  with our guard at the top
*)
let () =
  let len = Array.length Sys.argv in
  if len > 2 then
    begin
      let source_file = Sys.argv.(len - 1) in
      let test_file = source_file ^ "t" in
      let args = Array.sub Sys.argv 1 (len - 1) in
      if Sys.file_exists test_file then
        begin
          let source_chan = open_in source_file in
          let first_line  = input_line source_chan in
          let join_guard  = joined_header ~source_file ~test_file in
          let temp_name = source_file ^ ".tmp" in
          if starts_with join_guard first_line then
            begin
              close_in_noerr source_chan;
              call args
            end
          else
            begin
              let temp_chan = open_out temp_name in
              output_string temp_chan (join_guard ^ first_line);
              output_char temp_chan '\n';
              copy source_chan temp_chan;
              (* 0 will align the original .mlt source file because
                 we insert the IGNORE warning as the first line. *)
              let directive = Printf.sprintf "# 0 %S\n" test_file in
              output_string temp_chan directive;
              output_string temp_chan "(*BISECT-IGNORE-BEGIN*)\n";
              let test_chan = open_in test_file in
              copy test_chan temp_chan;
              output_string temp_chan "\n(*BISECT-IGNORE-END*)\n";
              close_in_noerr source_chan;
              close_in_noerr test_chan;
              close_out_noerr temp_chan;
              Sys.rename temp_name source_file;
              args.(len - 2) <- source_file;
              call args
            end
        end
      else
        call args
    end
  else
    begin
      Printf.eprintf "Error: invalid command-line\n";
      exit 1
    end
