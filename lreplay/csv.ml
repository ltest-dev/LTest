(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2013-2018                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  You may redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 3.                                                *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful, but WITHOUT     *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General      *)
(*  Public License for more details.                                      *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 3 for more          *)
(*  details (enclosed in the file LICENSE).                               *)
(*                                                                        *)
(**************************************************************************)

let separator_re = Str.regexp "[ \t]*,[ \t]*"

let input_line_opt input =
  try
    Some (input_line input)
  with End_of_file ->
    None

let rec read_stream ?(linenum=1) f input =
  match input_line_opt input with
  | None -> ()
  | Some line ->
    if line.[0] != '#' && String.length line > 0 then begin
      f linenum (Str.split_delim separator_re line)
    end;
    read_stream ~linenum:(linenum+1) f input

let read_file f filepath =
  let input = open_in filepath in
  read_stream f input;
  close_in input

let write_row output fields =
  begin match fields with
  | [] -> ()
  | head :: tail ->
    output_string output head;
    List.iter (fun field -> output_char output ','; output_string output field) tail;
    output_char output '\n'
  end
