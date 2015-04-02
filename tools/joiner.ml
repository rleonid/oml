(*
 * This utility is adapted from kaputt_pp.ml found in Kaputt, the associated
 * license is included below.
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
  Printf.sprintf "# 0 %S (* concating files for glory %S %S *)"
    source_file source_file test_file

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
          let temp_name, temp_chan = Filename.open_temp_file "joiner" ".ml" in
          if first_line = join_guard then
            begin
              close_in_noerr source_chan;
              close_out_noerr temp_chan;
              call args
            end
          else
            begin
              output_string temp_chan join_guard;
              output_char temp_chan '\n';
              (*let directive = Printf.sprintf "# 0 %S\n" source_file in
              output_string temp_chan directive; *)
              output_string temp_chan first_line;
              output_char temp_chan '\n';
              copy source_chan temp_chan;
              let directive = Printf.sprintf "# 1 %S\n" test_file in
              output_string temp_chan directive;
              let test_chan = open_in test_file in
              copy test_chan temp_chan;
              close_in_noerr source_chan;
              close_in_noerr test_chan;
              close_out_noerr temp_chan;
              Sys.rename temp_name source_file;
              args.(len - 3) <- source_file
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
