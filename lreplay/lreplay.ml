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

open Coverage

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

let usage_short ="Usage: lreplay [-update|-init|-stats] file.c [-drivers GLOB] [-main FUN]

Compute (hyper)label coverage based on a set of test drivers.
"
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

type mode = Update | Init | Stats




let on_source mainfun driverpatt mode force source =
  let labelsfile = (Filename.chop_extension source)^".labels" in
  let hypersfile = (Filename.chop_extension source)^".hyperlabels" in
  match mode with
  | Init ->
    LabelActions.init labelsfile source force
  (*| Check ->
    let report = Coverage.coverage ~force mainfun driverpatt source in
    ignore (LabelActions.check labelsfile (fst report))*)
  | Update ->
    let start_time = Unix.gettimeofday() in
    let report = Coverage.coverage ~force mainfun driverpatt source in
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




let main ?current argv =
  setup_env ();
  let usage = usage_short^"\nOptions:" in
  let mainfun = ref "*"
  and driverpatt = ref "${DIRNAME}/testcases_${BASENAME_NO_EXT}/${MAINFUN}/testdrivers/*.c"
  and rev_sources = ref []
  and mode = ref Update
  and force = ref false
  and lib_folder = ref ""
  and lib_options = ref ""
  and list_files = ref ""
  and includes_path = ref "." in
  let spec = Arg.align [
    "-update", Arg.Unit (fun () -> mode := Update), " Compute (hyper)label coverage, update .labels (and .hyperlabel) [action by default]";
    (*  "-check", Arg.Unit (fun () -> mode := Check), " Compute label coverage and check .labels"; *)
    "-init", Arg.Unit (fun () -> mode := Init)," Initialize .labels (do not run any test)";
    "-stats", Arg.Unit (fun () -> mode := Stats)," Print stats about .labels (do not run any test)";
    "-drivers", Arg.Set_string driverpatt, "GLOB Set driver file pattern (PathCrawler-specific default)";
    "-main", Arg.Set_string mainfun, "FUN Set main function name (default: *)";
    "-force", Arg.Set force, " Force recomputations (for -update) or overwriting (for -init)";
    "-compil-lib-folder", Arg.Set_string lib_folder, "Pass a library folder to gcc for compilation";
    "-compil-lib-options", Arg.Set_string lib_options, "Pass a list of libraries to use by gcc for compilation";
    "-compil-files", Arg.Set_string list_files, "Files to compile with drivers";
    "-compil-includes", Arg.Set_string includes_path, "Folder where headers are located"
    ]
  and add_sources source =
    rev_sources := source :: !rev_sources
  in

  try
    Arg.parse_argv ?current argv spec add_sources usage;
    Coverage.l_folder := !lib_folder;
    Coverage.l_options := !lib_options;
    Coverage.l_files := !list_files;
    Coverage.i_path := !includes_path;
    if !rev_sources = [] then
      (print_endline usage_short;
      print_endline "Run `lreplay --help` for more information.")
    else
      List.iter (on_source !mainfun !driverpatt !mode !force) (List.rev !rev_sources)
  with Arg.Bad error ->
    prerr_endline error
  | Arg.Help msg ->
    prerr_string msg;
    prerr_endline help_details

let () = main Sys.argv
