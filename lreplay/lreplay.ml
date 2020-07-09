(**************************************************************************)
(*                                                                        *)
(*  This file is part of LReplay.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2020                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Options


let on_source source =
  let labelsfile = (Filename.chop_extension source)^".labels" in
  let hypersfile = (Filename.chop_extension source)^".hyperlabels" in
  match mode with
  | Init ->
    LabelActions.init labelsfile source
  | Check ->
    let report = Coverage.coverage source in
    LabelActions.check labelsfile (fst report)
  | Update ->
    let start_time = Unix.gettimeofday() in
    let report = Coverage.coverage source in
    let labelstable = LabelActions.update labelsfile (fst report) in
    begin
      LabelActions.stats labelstable;
      Hyperlabels.hcoverage report hypersfile;
      let end_time = Unix.gettimeofday() in
      Format.eprintf "[lreplay] Coverage measurement time = %f s@." (end_time-.start_time);
    end
  | Stats ->
    if Sys.file_exists labelsfile then
      let data = Data.create () in
      Data.load data labelsfile;
      LabelActions.stats data
    else
      Format.eprintf "[lreplay] %s file does not exist@." labelsfile;
    Hyperlabels.stats hypersfile


let () =
  if rev_sources = [] then
    (print_endline usage_short;
     print_endline "Run `lreplay --help` for more information.")
  else
    List.iter on_source rev_sources
