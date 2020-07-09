(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
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
open Utils

let old_value = ref (Kernel.LogicalOperators.get ())


let store_label_data out annotations =
  (* TODO do that in its own module, ultimately shared with the other LTest-tools *)
  (* TODO (later) do something better than csv *)
  let formatter = Format.formatter_of_out_channel out in
  Format.fprintf formatter "# id, status, tag, origin_loc, current_loc, emitter, drivers, exec_time@.";
  let print_one (id, tags, cond, origin_loc) =
    let l = String.split_on_char '/' (print_file_path origin_loc) in
    let origin_file = List.nth l ((List.length l)-1) in
    let origin_line = (fst origin_loc).Filepath.pos_lnum in
    (* let us note obviously uncoverable labels as uncoverable
       (should only work when -lannot-simplify is on) *)
    let verdict = if Cil.isZero cond then "uncoverable" else "unknown" in
    Format.fprintf formatter "%d, %s, %s, %s:%d, , LAnnot, , 0.@." id verdict tags origin_file origin_line
  in
  List.iter print_one (List.rev annotations);
  Format.pp_print_flush formatter ()

let compute_outfile opt files =
  if opt = "" then
    if files = [] then
      "a_labels.out"
    else
      let base = Filepath.Normalized.to_pretty_string (List.hd files) in
      (* TODO check if it's not better to take the last filename *)
      let prefix = Filename.chop_extension base in
      let len_prefix = String.length prefix in
      let suffix = String.sub base len_prefix ((String.length base)-len_prefix) in
      prefix ^ "_labels" ^ suffix
  else
    opt


let annotate_on_project ann_names =
  let filename = compute_outfile (Options.Output.get ()) (Kernel.Files.get ()) in
  let basename = Filename.chop_extension filename in
  (* Remove .hyperlabels file if exists *)
  let hl_data_filename = basename ^ ".hyperlabels" in
  if Sys.file_exists hl_data_filename then
    Sys.remove hl_data_filename;

  let annotations = ref [] in
  let collect ann = annotations := ann :: !annotations in
  Annotators.annotate (compute_outfile (Options.Output.get ()) (Kernel.Files.get ())) ann_names ~collect (Ast.get ());

  let annotations = !annotations in
  (* output modified c file *)
  Options.feedback "write modified C file (to %s)" filename;
  let out = open_out filename in
  let formatter = Format.formatter_of_out_channel out in
  Utils.Printer.pp_file formatter (Ast.get ());
  Format.pp_print_flush formatter ();
  close_out out;
  (* output label data *)
  let data_filename = basename ^ ".labels" in
  Options.feedback "write label data (to %s)" data_filename;
  Options.feedback "%d labels/sequences created" (Annotators.getCurrentLabelId ());
  let out = open_out data_filename in
  store_label_data out annotations;
  close_out out;
  Options.feedback "finished"

let annotate ann_names =
  let base_project = Project.current () in
  let prj_name = (Project.get_name base_project) ^ "_labels" in
  let prj = Project.create_by_copy false prj_name in
  Options.debug "start project %s" prj_name;
  Project.on prj annotate_on_project ann_names

let setupMutatorOptions () =
  let f mutname =
    Options.debug ~level:2 "Enabling %s mutator" mutname;
    if mutname = "AOR" then L_wm.aorOption := true
    else if mutname = "COR" then L_wm.corOption := true
    else if mutname = "ABS" then L_wm.absOption := true
    else if mutname = "ROR" then L_wm.rorOption := true
    else Options.debug ~level:2 "Unknown mutators %s : Ignored" mutname;
  in
  Options.Mutators.iter f


(* ENTRY POINT *)
let run () =
  try
    setupMutatorOptions ();
    annotate (Datatype.String.Set.elements (Options.Annotators.get ()))
    (* Kernel.LogicalOperators.set !old_value *)
  with
  | Globals.No_such_entry_point _ ->
    Options.abort "`-main` parameter missing"
  | Dynamic.Unbound_value(s) -> Options.fatal "%s unbound" s
  | Dynamic.Incompatible_type(s) -> Options.fatal "%s incompatible" s
  | Failure s -> Options.fatal "unexpected failure: %s" s

let run_once, _ = State_builder.apply_once "LAnnotate.run" [] run

let help () =
  if Options.ListAnnotators.get () then begin
    Annotators.print_help Format.std_formatter;
    raise Cmdline.Exit
  end
let () = Cmdline.run_after_configuring_stage help

let run () =
  if not (Options.Annotators.is_empty ()) then run_once ()

let setup_run () =
  if not (Options.Annotators.is_empty ()) then begin
    Kernel.LogicalOperators.on () (* invalidate the Ast if any *)
  end

let () = Cmdline.run_after_configuring_stage setup_run

let () = Db.Main.extend run
