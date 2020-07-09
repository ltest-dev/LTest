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

let data_header = "# luncov-updated coverage data\n\
                   # id, status, tag, origin_loc, current_loc, emitter, drivers, exec_time\n";

type status = Unknown | Covered | Uncoverable

type id = int

type label = {
  mutable status: status;
  mutable tag: string;
  mutable origin_loc: string;
  mutable current_loc : string;
  mutable emitter: string;
  mutable drivers: string;
  mutable exec_time : float;
  mutable extra : string list;
}

type t = (id, label) Hashtbl.t

let status_of_string str =
  if str = "unknown" then Unknown
  else if str = "covered" then Covered
  else if str = "uncoverable" then Uncoverable
  else if str = "unreachable" then Uncoverable (* for backward compatibility *)
  else invalid_arg "status_of_string"

let string_of_status s =
  match s with
  | Unknown -> "unknown"
  | Covered -> "covered"
  | Uncoverable -> "uncoverable"

let string_of_statusAT s =
  match s with
  | Unknown -> "unknown"
  | Covered -> "covered"
  | Uncoverable -> "always true"

let pp_status formatter status =
  Format.pp_print_string formatter (if !Instrument_label.at=Cil_types.Req then string_of_status status else string_of_statusAT status)

let create ?(size=100) () = Hashtbl.create size

let size = Hashtbl.length

let input_line_opt input =
  try
    Some (input_line input)
  with End_of_file ->
    None

let load_add data idstr statusstr tag origin_loc current_loc emitter drivers exec_time extra replace line =
  try
    let id = int_of_string idstr in
    let status = status_of_string statusstr in
    if replace then
      Hashtbl.replace data id {status; tag; origin_loc; current_loc; emitter; drivers; exec_time; extra}
    else if not (Hashtbl.mem data id) then
      Hashtbl.add data id {status; tag; origin_loc; current_loc; emitter; drivers; exec_time; extra}
    else
      Options.warning "duplicate id (row ignored) at line %d in .labels file" line
  with Invalid_argument _ ->
    Options.warning "incorrect id (%s) or status (%s) at line %d in .labels" idstr statusstr line

let rec load_stream replace linenum input data =
  match input_line_opt input with
  | None -> ()
  | Some line ->
    if line.[0] <> '#' && String.length line > 0 then begin

      let fields = Str.split_delim (Str.regexp "[ \t]*,[ \t]*") line in
      match fields with
      | [] | _::[] ->
        Options.warning "not enough fields at line %d in .labels file" linenum
      | idstr :: statusstr :: tag :: origin_loc :: current_loc :: emitter :: drivers ::exec_time :: extra ->
        load_add data idstr statusstr tag origin_loc current_loc emitter drivers (float_of_string exec_time) extra replace linenum
      | idstr :: statusstr :: tag :: origin_loc :: current_loc :: emitter :: drivers :: extra ->
        load_add data idstr statusstr tag origin_loc current_loc emitter drivers 0.0 extra replace linenum
      | idstr :: statusstr :: tag :: origin_loc :: current_loc :: emitter :: extra ->
        load_add data idstr statusstr tag origin_loc current_loc emitter "" 0.0 extra replace linenum
      | idstr :: statusstr :: tag :: origin_loc :: current_loc :: extra ->
        load_add data idstr statusstr tag origin_loc current_loc "" "" 0.0 extra replace linenum
      | idstr :: statusstr :: tag :: origin_loc :: [] ->
        load_add data idstr statusstr tag origin_loc "" "" "" 0.0 [] replace linenum
      | idstr :: statusstr :: tag :: [] ->
        load_add data idstr statusstr tag "" "" "" "" 0.0 [] replace linenum
      | idstr :: statusstr :: [] ->
        load_add data idstr statusstr "" "" "" "" "" 0.0 [] replace linenum
    end;
    load_stream replace (linenum+1) input data

let load data ?(replace=false) filepath =
  let input = open_in filepath in
  load_stream replace 1 input data;
  close_in input

let store data filepath =
  if Options.Backup.get () then Commons.backup filepath;
  let output = open_out filepath in
  let f id {status; tag; origin_loc; current_loc; emitter; drivers; exec_time; extra} acc =
    let drivers = if status = Covered then drivers else "" in
    (id, (String.concat ", " (string_of_int id :: string_of_status status :: tag
                              :: origin_loc :: current_loc :: emitter :: drivers
                              :: (string_of_float exec_time) :: extra ))) :: acc
  in
  let l = Hashtbl.fold f data [] in
  let l = List.sort compare l in
  output_string output data_header;
  List.iter (fun (_,line)-> output_string output line; output_char output '\n') l;
  close_out output

let storeAT data filepath =
  if Options.Backup.get () then Commons.backup filepath;
  let output = open_out filepath in
  let f id {status; tag; origin_loc; current_loc; emitter; drivers; exec_time; extra} acc =
    let drivers = if status = Covered then drivers else "" in
    (id, (String.concat ", " (string_of_int id :: string_of_statusAT status :: tag
                              :: origin_loc :: current_loc :: emitter :: drivers
                              :: (string_of_float exec_time) :: extra ))) :: acc
  in
  let l = Hashtbl.fold f data [] in
  let l = List.sort compare l in
  output_string output data_header;
  List.iter (fun (_,line)-> output_string output line; output_char output '\n') l;
  close_out output


let merge_status ?(force=false) old_status new_status =
  match old_status,new_status with
  | Covered, Uncoverable ->
    Options.warning "discrepency detected (covered detected as uncoverable)";
    if force then new_status else old_status
  | Covered, Unknown ->
    old_status
  | Uncoverable, Unknown ->
    Options.warning "loss of precision detected";
    if force then new_status else old_status
  | _, _ ->
    new_status

let merge_tag ?(force=false) old_tag new_tag =
  if old_tag = "" || force then new_tag
  else old_tag

let merge_loc = merge_tag

let merge_extra ?(force=false) old_extra new_extra =
  if old_extra = [] || force then new_extra
  else old_extra

let merge_status_info ?(force=false) label status emitter time =
  let new_status = merge_status ~force label.status status in
  if force || new_status <> label.status then
    begin
      label.emitter <- emitter;
      label.status <- new_status;
      Extlib.may (fun time ->
          if Options.Time.get () then
            label.exec_time <- time
          else
            label.exec_time <- 0.
        ) time
    end
  else if label.emitter = "" || label.exec_time = 0.0 then (* same status: put the emitter and time if there is none *)
    begin
      label.emitter <- emitter;
      Extlib.may (fun time ->
          if Options.Time.get () then
            label.exec_time <- time
          else
            label.exec_time <- 0.
        ) time
    end


let update_status ?(force=false) label status emitter time =
  match status, emitter with
  | Some status, None ->
    Options.warning "status updated without emitter";
    merge_status_info ~force label status "anonymous" time
  | Some status, Some emitter ->
    Options.debug ~level:3 "update label: %a from %s with force=%b"
      pp_status status emitter force;
    merge_status_info ~force label status emitter time
  | _, _ ->
    ()

let default () =
  { status = Unknown; tag = ""; origin_loc = ""; current_loc = ""; emitter = ""; drivers = ""; exec_time = 0.0; extra = [] }

let update data ?(force=false) ?status ?tag ?origin_loc ?current_loc ?emitter ?exec_time ?extra id =
  let label =
    if Hashtbl.mem data id then
      Hashtbl.find data id
    else
      let l = default () in
      Hashtbl.add data id l;
      l
  in
  update_status ~force label status emitter exec_time;
  (* Extlib.may (fun status -> label.status <- merge_status ~force label.status status) status; *)
  (* Extlib.may (fun emitter -> label.emitter <- merge_emitter ~force label.emitter emitter) emitter; *)
  (* Extlib.may (fun exect -> label.exec_time <- merge_exec_time ~force label.exec_time exect) exec_time; *)
  Extlib.may (fun tag -> label.tag <- merge_tag ~force label.tag tag) tag;
  Extlib.may (fun loc -> label.origin_loc <- merge_loc ~force label.origin_loc loc) origin_loc;
  Extlib.may (fun loc -> label.current_loc <- merge_loc ~force label.current_loc loc) current_loc;
  Extlib.may (fun extra -> label.extra <- merge_extra ~force label.extra extra) extra

let is_unknown data id =
  not (Hashtbl.mem data id) || (Hashtbl.find data id).status = Unknown

let is_uncoverable data id =
  (Hashtbl.mem data id) && (Hashtbl.find data id).status = Uncoverable

let get_status data id =
  (Hashtbl.find data id).status

let get_emitter data id =
  (Hashtbl.find data id).emitter

let get_exec_time data id =
  (Hashtbl.find data id).exec_time

module S = Datatype.Int.Set

let get_label_ids data =
  S.elements (Hashtbl.fold (fun k _ acc -> S.add k acc) data S.empty)

type stats = {
  total: int;
  unknown : int;
  covered : int;
  uncoverable: int;
}

let get_stats data =
  let total = ref 0 in
  let unknown = ref 0 in
  let covered = ref 0 in
  let uncoverable = ref 0 in
  let incr_counter = function
    | Unknown -> incr unknown
    | Covered -> incr covered
    | Uncoverable -> incr uncoverable
  in
  Hashtbl.iter (fun _ li -> incr_counter li.status; incr total) data;
  let total = !total in
  let unknown = !unknown in
  let covered = !covered in
  let uncoverable = !uncoverable in
  { total; unknown; covered; uncoverable }

let coverage_ratio stats =
  let maybe_coverable = stats.total - stats.uncoverable in
  if maybe_coverable > 0 then
    Some (float_of_int stats.covered /. float_of_int maybe_coverable)
  else
    None

let pp_stats f stats =
  Format.fprintf f "total number of labels %8d@." stats.total;
  Format.fprintf f "number of covered      %8d@." stats.covered;
  Format.fprintf f "number of uncoverable  %8d@." stats.uncoverable;
  Format.fprintf f "number of unknown      %8d@." stats.unknown;
  let pcov rat =
    Format.fprintf f "coverage ratio         %8.2f%% \
                      (covered over maybe coverable)@."
      (100.0 *. rat)
  in
  Extlib.may pcov (coverage_ratio stats)

let pp_diff_stats f (before,after) =
  let pp_diff f n = if n <> 0 then Format.fprintf f " (%+d)" n in
  Format.fprintf f "total number of labels %8d%a@." after.total
    pp_diff (after.total-before.total);
  Format.fprintf f "number of covered      %8d%a@." after.covered
    pp_diff (after.covered-before.covered);
  Format.fprintf f "number of uncoverable  %8d%a@." after.uncoverable
    pp_diff (after.uncoverable-before.uncoverable);
  Format.fprintf f "number of unknown      %8d%a@." after.unknown
    pp_diff (after.unknown-before.unknown);
  match coverage_ratio before, coverage_ratio after with
  | Some before_rat, Some after_rat ->
    Format.fprintf f "coverage ratio         %8.2f%% \
                      (was %.2f%%)@."
      (100.0 *. after_rat) (100.0 *. before_rat)
  | None, Some after_rat ->
    Format.fprintf f "coverage ratio         %8.2f%% \
                      (covered over maybe coverable)@."
      (100.0 *. after_rat)
  | _ -> ()


let clear data =
  Hashtbl.clear data
