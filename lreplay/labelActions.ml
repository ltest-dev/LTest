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

open Coverage

let check labelsfile report =
  let labelstable = Data.create () in
  Data.load labelstable labelsfile;
  let errors = ref 0 in
  let f id row =
    try
      let status = Data.get_status labelstable id in
      match row.covered, status with
      | true, Data.Covered -> ()
      | true, _ ->
          incr errors;
          Format.eprintf "[lreplay error] label #%d incorrectly reported as %a in %s
  covered in %s@." id Data.pp_status status labelsfile row.driver

      | false, (Data.Unknown|Data.Uncoverable) -> ()
      | false, _ ->
        incr errors;
        Format.eprintf "[lreplay error] label #%d incorrectly reported as %a in %s
  reached (but not covered) in %s@." id Data.pp_status status labelsfile row.driver
    with Not_found ->
      incr errors;
      Format.eprintf "[lreplay error] missing label #%d in %s @." id labelsfile
  in
  Hashtbl.iter f report;
  let g id label =
    if not (Hashtbl.mem report id) && label.Data.status = Data.Covered then begin
      incr errors;
      Format.eprintf "[lreplay error] label #%d incorrectly reported as covered in %s
  uncovered and unreached in all tests@." id labelsfile
    end
  in
  Data.iter g labelstable;
  if !errors > 0 then
    Format.eprintf "[lreplay] found %d error(s) in %s@." !errors labelsfile
  else
    Format.eprintf "[lreplay] found no error in %s@." labelsfile


let update labelsfile report =
  let labelstable = Data.create ~size:(Hashtbl.length report) () in
  if Sys.file_exists labelsfile then Data.load labelstable labelsfile;
  let errors = ref 0 in
  let changes = ref 0 in
  let f id row =
    try
      let status = Data.get_status labelstable id in
      let nstatus =
        match row.covered, status with
        | _, Data.Unreachable ->
          incr errors;
          Format.eprintf "[lreplay error] label #%d was incorrectly reported as unreachable in %s\n  %s in %s@." id labelsfile (if row.covered then "covered" else "reached") row.driver;
          if row.covered then Data.Covered else Data.Unknown
        | true, Data.Uncoverable ->
          incr errors;
          Format.eprintf "[lreplay error] label #%d was incorrectly reported as uncoverable in %s\n  covered in %s@." id labelsfile row.driver;
          Data.Covered
        | true, _ -> Data.Covered
        | _, _ -> status
      in
      if status <> nstatus then begin
          incr changes;
          Data.update labelstable id ~tag:row.tag ~current_loc:row.loc nstatus
        end
      else if nstatus = Covered then begin
        Data.check_tag labelstable id row.tag;
        incr changes
        end;
    with Not_found ->
      incr errors;
      incr changes;
      Format.eprintf "[lreplay error] label #%d was missing in %s@." id labelsfile;
      Data.add labelstable id ~tag:row.tag ~current_loc:row.loc
        (if row.covered then Data.Covered else Data.Unknown)
  in
  Hashtbl.iter f report;
  let g id label =
    if not (Hashtbl.mem report id) && label.Data.status = Data.Covered then begin
      incr errors;
      incr changes;
      Format.eprintf "[lreplay error] label #%d was incorrectly reported as covered in %s
  uncovered and unreached in all tests@." id labelsfile;
      Data.set_status labelstable id Data.Unknown
    end
  in
  Data.iter g labelstable;
  if !changes > 0 then begin
    Data.store labelsfile labelstable;
    Format.eprintf "[lreplay] Changes written to %s@." labelsfile;
  end;
  if !errors > 0 then
    Format.eprintf "[lreplay] %d error found and fixed in %s@." !errors labelsfile;
  labelstable

let mk_status_counters () : (Data.status, int ref) Hashtbl.t =
  Hashtbl.create 5
let incr_status_counter counters status =
  try
    incr (Hashtbl.find counters status)
  with Not_found ->
    Hashtbl.add counters status (ref 1)

let incr_tag_counter tag_counters status tag =
   let status_counters =
    try
      Hashtbl.find tag_counters tag
    with Not_found ->
      let res = mk_status_counters () in
      Hashtbl.add tag_counters tag res;
      res
  in
  incr_status_counter status_counters status

let pp_counters name counters =
  let total = Hashtbl.fold (fun _ v acc -> !v + acc) counters 0 in
  let covered = try !(Hashtbl.find counters Data.Covered) with Not_found -> 0 in
  let uncoverable = try !(Hashtbl.find counters Data.Uncoverable) with Not_found -> 0 in
  let unreachable = try !(Hashtbl.find counters Data.Unreachable) with Not_found -> 0 in
  Format.eprintf "[lreplay] %s\n" name;
  Format.eprintf "  total number of labels  % 8d@." total;
  Hashtbl.iter (fun status i ->
    Format.eprintf "  number of %- 12s  % 8d@." (Data.string_of_status status) !i
  ) counters;
  if total > 0 then
    Format.eprintf "  coverage ratio          % 8d%%  (covered/maybe coverable)@." (covered*100 / (total-uncoverable-unreachable))


let stats labelstable =
  let tag_counters = Hashtbl.create 5 in
  Hashtbl.add tag_counters "" (mk_status_counters ());

  let incr_counters _id label =
    let tags = "" :: (StrUtils.explode label.Data.tag " \t") in
    let status = label.Data.status in
    List.iter (incr_tag_counter tag_counters status) tags
  in
  Data.iter incr_counters labelstable;
  pp_counters "global statistics" (Hashtbl.find tag_counters "");
  Hashtbl.remove tag_counters "";
  Hashtbl.iter
    (fun tag counters -> pp_counters ("statistics for "^tag) counters)
    tag_counters


let grep_pattern = "@@@LABEL(.*)LABEL@@@"

let sed_script = "
s/^@@@LABEL(//
s/)LABEL@@@//
s/\"$//
s/\"\\? *, *\"\\?/,/g
"

let init labelsfile source =
  if not Options.force && Sys.file_exists labelsfile then
    Format.eprintf "[lreplay error] %s already exists (use -force to overwrite)@." labelsfile
  else
  let input = Unix.open_process_in (String.concat " " [
    "$CPP";
    "$CPPFLAGS";
    "-include";
    StrUtils.shell_escape (ForeignCode.detection_h ());
    StrUtils.shell_escape source;
    "| grep -o";
    StrUtils.shell_escape grep_pattern;
    "| sed";
    StrUtils.shell_escape sed_script
  ]) in
  let table = Data.create () in
  let add _line fields =
      match fields with
      | idstr :: file :: line :: tag :: _ ->
        let current_loc = file^":"^line in
        begin try
          let id = int_of_string idstr in
          Data.add table id ~current_loc ~tag Data.Unknown
        with Invalid_argument _ ->
          Format.eprintf "[lreplay error] invalid id (%s) in %s@." idstr source
        end
      | _ -> ()
  in
  Csv.read_stream add input;
  begin match Unix.close_process_in input with
  | Unix.WEXITED 0 -> ()
  | _ -> Format.eprintf "[lreplay error] preprocessor process did not exit properly@."
  end;
  Data.store labelsfile table;
  Format.eprintf "[lreplay] %s initialized@." labelsfile
