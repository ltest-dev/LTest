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

type mode = Update | Check | Init | Stats

let usage_short ="Usage: lreplay [-update|-init|-stats] file.c [-drivers GLOB] [-main FUN]\nCompute (hyper)label coverage based on a set of test drivers."

let usage = usage_short^"\nOptions:"
let mainfun = ref "*"
let driverpatt = ref "${DIRNAME}/testcases_${BASENAME_NO_EXT}/${MAINFUN}/testdrivers/*.c"
let rev_sources = ref []
let mode = ref Update
let force = ref false
let backup = ref false
let lib_folder = ref ""
let lib_options = ref ""
let list_files = ref ""
let includes_path = ref "."
let other_options = ref ""

let putenv_default name value =
  try
    ignore (Unix.getenv name)
  with Not_found ->
    Unix.putenv name value

let setup_env () =
  putenv_default "CC"        "gcc";
  putenv_default "CCFLAGS"   "";
  putenv_default "CPP"       "gcc -E -P";
  putenv_default "CPPFLAGS"  ""


let help_details ="
The key option is '-drivers'. It specifies a pattern to find test drivers.
The pattern may contain *, ?, [...] like the shell, and variables of the form
${NAME} where NAME is one of SOURCE, DIRNAME, BASENAME, BASENAME_NO_EXT,
and MAINFUN. The default value is specific to PathCrawler:
  ${DIRNAME}/testcases_${BASENAME_NO_EXT}/${MAINFUN}/testdrivers/TC_*.c.

The MAINFUN variable indicates the function under test (-main in Frama-C). By
default it's set to *, that is every possible function for which PathCrawler
had been run. You may change it via the '-main' flag.

You may override the default C compiler (also used as linker, default: gcc)
and C preprocessor (default: gcc -E -P) with environment variables CC and CPP. You
may also specify additional flags with CCFLAGS and CPPFLAGS.
"

let specs =
  [ "-update", Arg.Unit (fun () -> mode := Update), " Compute (hyper)label coverage, update .labels (and .hyperlabel) [action by default]";
    "-check", Arg.Unit (fun () -> mode := Check), " Compute label coverage and check .labels";
    "-init", Arg.Unit (fun () -> mode := Init)," Initialize .labels (do not run any test)";
    "-stats", Arg.Unit (fun () -> mode := Stats)," Print stats about .labels (do not run any test)";
    "-drivers", Arg.Set_string driverpatt, "GLOB Set driver file pattern (PathCrawler-specific default)";
    "-main", Arg.Set_string mainfun, "FUN Set main function name (default: *)";
    "-force", Arg.Set force, " Force recomputations (for -update) or overwriting (for -init)";
    "-backup", Arg.Set backup, " create .labels backup before storing labels update";
    "-compil-lib-folder", Arg.Set_string lib_folder, "Pass a library folder to gcc for compilation";
    "-compil-lib-options", Arg.Set_string lib_options, "Pass a list of libraries to use by gcc for compilation";
    "-compil-files", Arg.Set_string list_files, "Files to compile with drivers";
    "-compil-includes", Arg.Set_string includes_path, "Folder where headers are located";
    "-other-options", Arg.Set_string other_options, "Other options added when compiling"
    ]

let alspecs = Arg.align specs

let add_sources source =
  rev_sources := source :: !rev_sources

let () =
  setup_env ();
  try
    Arg.parse alspecs add_sources usage;
    rev_sources := List.rev !rev_sources;
  with Arg.Bad error ->
        prerr_endline error
     | Arg.Help msg ->
        prerr_string msg;
        prerr_endline help_details

let mainfun = !mainfun
let driverpatt = !driverpatt
let rev_sources = !rev_sources
let mode = !mode
let force = !force
let backup = !backup
let lib_folder = !lib_folder
let lib_options = !lib_options
let list_files = !list_files
let other_options = !other_options
let includes_path = !includes_path
