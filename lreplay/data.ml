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

type status = Unknown | Covered | Uncoverable | Unreachable

type id = int

type label = {
  mutable status: status;
  mutable tag: string;
  mutable origin_loc: string;
  mutable current_loc: string;
  mutable emitter: string;
  mutable drivers: string list;
  mutable extra: string list;
}

type t = (id, label) Hashtbl.t

let status_of_string str =
  if str = "unknown" then Unknown
  else if str = "covered" then Covered
  else if str = "uncoverable" then Uncoverable
  else if str = "unreachable" then Unreachable
  else invalid_arg "status_of_string"

let string_of_status st =
  match st with
  | Unknown -> "unknown"
  | Covered -> "covered"
  | Uncoverable -> "uncoverable"
  | Unreachable -> "unreachable"

let pp_status formatter status =
  Format.pp_print_string formatter (string_of_status status)

let create ?(size=100) () = Hashtbl.create size
let size = Hashtbl.length

let get_drivers id =
  let list_drivers = (List.sort compare (Coverage.StringSet.elements (try (Hashtbl.find (Coverage.labelstotest) (string_of_int id)) with Not_found -> Coverage.StringSet.empty))) in
  list_drivers

let load_add data idstr statusstr tag origin_loc current_loc emitter drivers extra replace file line =
  try
    let id = int_of_string idstr in
    let status = status_of_string statusstr in
    (* a quoi ça sert? *)
    if replace then
      let drivers = if status <> Covered then [] else drivers in
      Hashtbl.replace data id {status; tag; origin_loc; current_loc; emitter; drivers; extra}
    else if not (Hashtbl.mem data id) then
      (* If we covered the label with new drivers, then add them to the current list
         of drivers instead of removing the old ones *)
      let old_count = List.length drivers in
      let drivers = List.sort_uniq compare (List.append drivers (get_drivers id)) in
      (* If the list is longer (i.e. if new drivers covered the label,
         then make sure the emitter is LReplay *)
      let emitter = if old_count < List.length drivers then "LReplay" else emitter in
      Hashtbl.add data id {status; tag; origin_loc; current_loc; emitter; drivers; extra}
    else
      Format.eprintf "[.labels] duplicate id (row ignored) at %s:%d@." file line
  with Invalid_argument _ ->
    Format.eprintf "[.labels] incorrect id (%s) or status (%s) at %s:%d@." idstr statusstr file line

let load ?(replace=false) data labelsfile =
  let f linenum fields =
    match fields with
    | idstr :: statusstr :: tag :: origin_loc :: current_loc :: emitter :: drivers :: extra ->
      load_add data idstr statusstr tag origin_loc current_loc emitter (Str.split (Str.regexp "[ \t]+") drivers) extra replace labelsfile linenum
    | idstr :: statusstr :: tag :: origin_loc :: current_loc :: emitter :: extra ->
      load_add data idstr statusstr tag origin_loc current_loc emitter [] extra replace labelsfile linenum
    | idstr :: statusstr :: tag :: origin_loc :: current_loc :: [] ->
      load_add data idstr statusstr tag origin_loc current_loc "" [] [] replace labelsfile linenum
    | idstr :: statusstr :: tag :: origin_loc :: [] ->
      load_add data idstr statusstr tag origin_loc "" "" [] [] replace labelsfile linenum
    | idstr :: statusstr :: tag :: [] ->
      load_add data idstr statusstr tag "" "" "" [] [] replace labelsfile linenum
    | idstr :: statusstr ::  [] ->
      load_add data idstr statusstr "" "" "" "" [] [] replace labelsfile linenum
    | _ ->
      Format.eprintf "[.labels] invalid row (at least 2 fields expected) at %s:%d@." labelsfile linenum
  in
  Csv.read_file f labelsfile

let rec numbered_backup n filepath =
  let filepathn = filepath^"."^string_of_int n in
  if Sys.file_exists filepathn then
    numbered_backup (n+1) filepath
  else
    Sys.rename filepath filepathn

let backup_if_needed filepath =
  if Options.backup && Sys.file_exists filepath then
    numbered_backup 1 filepath

let store labelsfile table =
  backup_if_needed labelsfile;
  let output = open_out labelsfile in
  let f id label acc =
    (id, string_of_int id :: string_of_status label.status :: label.tag :: label.origin_loc :: label.current_loc :: label.emitter :: (String.concat " " label.drivers) :: label.extra) :: acc
  in
  let l = Hashtbl.fold f table [] in
  let l = List.sort (fun (id1, _) (id2, _) -> compare id1 id2) l in
  output_string output "# LReplay-updated coverage data\n";
  output_string output "# id, status, tag, origin_loc, current_loc, emitter, drivers, exec_time\n";
  List.iter (fun (_,fields) -> Csv.write_row output fields) l;
  close_out output

let iter f table =
  Hashtbl.iter f table

let get table id =
  let l = Hashtbl.find table id in
  l

let get_status table id =
  let l = Hashtbl.find table id in
  l.status

let set_status table id status =
  let l = Hashtbl.find table id in
  l.status <- status

let add table id ?(tag="") ?(current_loc="") ?(extra=["0."]) status =
  if Hashtbl.mem table id then failwith "add_label";
  let current_loc = StrUtils.get_file_from_path current_loc in
  let drivers = if status <> Covered then [] else get_drivers id in
  Hashtbl.add table id {status; tag; origin_loc=""; current_loc; emitter="LReplay"; drivers; extra}

let update data id ?(tag="") ?(current_loc="") ?(extra=[]) newstatus =
  let current_loc = StrUtils.get_file_from_path current_loc in
  if Hashtbl.mem data id then begin
    let label = Hashtbl.find data id in
    let old = label.status in
    match old,newstatus with
    | Covered, (Unreachable|Uncoverable) ->
      Format.eprintf "[warning] discrepency detected (covered detected as uncoverable) for label #%d@." id
    | Covered, Unknown -> () (* let it be*)
    | (Uncoverable|Unreachable), Unknown
    | Unreachable, Uncoverable ->
      Format.eprintf "[warning] loss of precision detected for label id #%d@." id
    | _, _ ->
      label.status <- newstatus;
      label.emitter <- "LReplay";
      if label.tag = "" then label.tag <- tag;
      if label.current_loc = "" then label.current_loc <- current_loc
  end
  else
    let drivers = if newstatus <> Covered then [] else get_drivers id in
    Hashtbl.add data id {status=newstatus; tag; origin_loc=""; current_loc; emitter="LReplay"; drivers; extra}

(* Pas très opti, à revoir
   Quand pathcrawler crée des labels, le tag n'est pas inclu
   --> Modifier pathcrawler pour garder le Tag ?
*)
let check_tag data id tag =
  if Hashtbl.mem data id then begin
    let label = Hashtbl.find data id in
    if label.tag = "" then label.tag <- tag
  end
