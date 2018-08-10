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

let store_label_data out annotations =
  (* TODO do that in its own module, ultimately shared with the other LTest-tools *)
  (* TODO (later) do something better than csv *)
  let formatter = Format.formatter_of_out_channel out in
  Format.fprintf formatter "# id, status, tag, origin_loc, current_loc, emitter, exec_time@.";
  let print_one (id, tags, cond, origin_loc) =
    let l = Transitioning.String.split_on_char '/' ((fst origin_loc).Lexing.pos_fname) in
    let origin_file = List.nth l ((List.length l)-1) in
    let origin_line = (fst origin_loc).Lexing.pos_lnum in
    (* let us note obviously uncoverable labels as uncoverable
       (should only work when -lannot-simplify is on) *)
    let verdict = if Cil.isZero cond then "uncoverable" else "unknown" in
    Format.fprintf formatter "%d,%s,%s,%s:%d,,lannot,0.@." id verdict tags origin_file origin_line
  in
  List.iter print_one (List.rev annotations);
  Format.pp_print_flush formatter ()

let compute_outfile opt files =
  if opt = "" then
    if files = [] then
      "a_labels.out"
    else
      let base = List.hd files in
      (* TODO check if it's not better to take the last filename *)
      let prefix = Filename.chop_extension base in
      let len_prefix = String.length prefix in
      let suffix = String.sub base len_prefix ((String.length base)-len_prefix) in
      prefix ^ "_labels" ^ suffix
  else
    opt


let annotate_on_project ann_names =
  Kernel.LogicalOperators.on (); (* invalidate the Ast if any *)
  let filename = compute_outfile (Options.Output.get ()) (Kernel.Files.get ()) in

  (* Remove .hyperlabels file if exists *)
  let hl_data_filename = (Filename.chop_extension filename) ^ ".hyperlabels" in
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
  let data_filename = (Filename.chop_extension filename) ^ ".labels" in
  Options.feedback "write label data (to %s)" data_filename;
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
    if mutname = "AOR" then Wm.aorOption := true
    else if mutname = "COR" then Wm.corOption := true
    else if mutname = "ABS" then Wm.absOption := true
    else if mutname = "ROR" then Wm.rorOption := true
    else Options.debug ~level:2 "Unknown mutators %s : Ignored" mutname;
  in
  Options.Mutators.iter f


(* ENTRY POINT *)
let run () =
  try
    setupMutatorOptions ();
    annotate (Datatype.String.Set.elements (Options.Annotators.get ()))
  with
  | Globals.No_such_entry_point _ ->
    Options.abort "`-main` parameter missing"
  | Dynamic.Unbound_value(s) -> Options.fatal "%s unbound" s
  | Dynamic.Incompatible_type(s) -> Options.fatal "%s incompatible" s
  | Failure s -> Options.fatal "unexpected failure: %s" s

let run () =
  if Options.ListAnnotators.get () then
    begin
      Annotators.print_help Format.std_formatter;
      exit 0;
    end
  else if not (Options.Annotators.is_empty ()) then
    run ();
  Kernel.LogicalOperators.off ()

let () =
  Db.Main.extend run
