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

type row = {
  id: int;
  covered: bool;
  tag: string;
  loc: string;
  driver: string;
  bindings: (string * string) list;
}

(**
   Detailed report
*)
type detailed_report = row list

(**
   Simplified report
*)
type report = (int, row) Hashtbl.t


let escape_slash =
  let re = Str.regexp "[/\\.]" in
  Str.global_replace re "_"

let newer_than file1 file2 =
  try
    let stat1 = Unix.stat file1 and stat2 = Unix.stat file2 in
    stat1.Unix.st_mtime > stat2.Unix.st_mtime
  with Unix.Unix_error (Unix.ENOENT, _, _) ->
    false

let compile_test_driver file binary output_file output_file_seq =
  let compile_cmd = String.concat " " [
      "$CC";
      "$CPPFLAGS $CFLAGS";
      "-I" ^ includes_path;
      "-g"; (* For valgrind && gdb *)
      lib_folder;
      "-include "^StrUtils.shell_escape (ForeignCode.uthash_h ());
      "-include "^StrUtils.shell_escape (ForeignCode.utlist_h ());
      "-include "^StrUtils.shell_escape (ForeignCode.runtime_h ());
      "-DLREPLAY_OUTPUT="^StrUtils.shell_escape output_file;
      "-DSREPLAY_OUTPUT="^StrUtils.shell_escape output_file_seq;
      "-DLREPLAY_TESTDRIVER="^StrUtils.shell_escape file;
      StrUtils.shell_escape (ForeignCode.runtime_c ());
      StrUtils.shell_escape file;
      list_files;
      lib_options;
      other_options;
      " -o "^StrUtils.shell_escape binary;
      (*  " 1>/dev/null 2>/dev/null " *)
    ] in
  (* Format.printf "[lreplay] compile test driver %s@." binary; *)
  (* prerr_endline compile_cmd; *)
  let status = Sys.command compile_cmd in
  if status <> 0 then
    failwith ("Cannot compile test driver " ^  compile_cmd);
  ignore (status = status)

let run_test_driver binary output_file =
  let status = Sys.command ((StrUtils.shell_escape binary) (* ^ " 1>/dev/null 2>/dev/null " *) ) in
  if status <> 0 then
    Format.eprintf "[lreplay] warning %s failed with exit code %d@." binary status
(*  ignore(Sys.command ("mv ./a.covlabels " ^ output_file)  ) *)



let rec make_bindings l file linenum =
  match l with
  | variable :: value :: tail ->  (variable,value) :: (make_bindings tail file linenum)
  | [] -> []
  | _ -> Format.eprintf "[lreplay warning] invalid row (2n+1 fields expected for bindings) at %s:%d@." file linenum;
    raise (Invalid_argument "")


module StringSet = Set.Make (struct
    type t = string
    let compare = compare
  end)

module StringSetSet = Set.Make (struct
    type t = StringSet.t
    let compare = StringSet.compare
  end)

let labelstotest = (Hashtbl.create 1000)

let compute_imply () =
  let the_list = ref [] in
  Hashtbl.iter (fun l1 t1 -> Hashtbl.iter (fun l2 t2 ->
      if l1 <> l2 && StringSet.subset t1 t2 then the_list := (l1 ^ " in " ^ l2)::!the_list
    ) labelstotest ) labelstotest;
  !the_list

let compute_equiv () =
  let final =  (Hashtbl.create 1000) in
  let groups = (Hashtbl.create 1000) in
  let groups_set = ref StringSetSet.empty in
  Hashtbl.iter (fun l t -> groups_set := StringSetSet.add t !groups_set; Hashtbl.add groups t l) labelstotest;
  StringSetSet.iter (fun t -> let l = Hashtbl.find_all groups t in if List.length l > 1 then Hashtbl.add final t l) !groups_set;
  final

let print_lst_li oc l = List.iter (Printf.fprintf oc "%s\n") l

let print_htbl_t_l oc h = Hashtbl.iter (fun _ l -> Printf.fprintf oc "%s\n" ("[" ^ (String.concat "," l ^ "]") )) h

let saveLabelTest l t =
  let old_set =
    try
      Hashtbl.find labelstotest (string_of_int l)
    with Not_found -> StringSet.empty
  in
  let new_set = StringSet.add t old_set in
  Hashtbl.replace labelstotest (string_of_int l) new_set


exception Unimplemented of string

module IntSet = Set.Make (struct
    type t = int
    let compare = compare
  end)


let coveredSequences = Hashtbl.create 100
let partiallyCoveredSequences = Hashtbl.create 100
let invertedPartiallyCoveredSequences = Hashtbl.create 100

let add_inverted tag ids =
  if Hashtbl.mem invertedPartiallyCoveredSequences tag then
    let l = Hashtbl.find invertedPartiallyCoveredSequences tag in
    if not (List.exists (fun id -> id = ids) l) then
      Hashtbl.replace invertedPartiallyCoveredSequences tag (ids::l);
  else
    Hashtbl.add invertedPartiallyCoveredSequences tag [ids]

let check_inverted tag =
  if Hashtbl.mem invertedPartiallyCoveredSequences tag then begin
    List.iter (fun id -> (
          Hashtbl.remove partiallyCoveredSequences id)
      ) (Hashtbl.find invertedPartiallyCoveredSequences tag);
    Hashtbl.remove invertedPartiallyCoveredSequences tag
  end

let load_test_driver_coverage_seq file driver =
  let f linenum fields =
    try
      match fields with
      | id :: [] ->
        let idSeq = int_of_string id in
        (*L'info tag est inutile, donc je l'ai supprimé pour économiser de la place *)
        if not (Hashtbl.mem coveredSequences idSeq) then
          Hashtbl.add coveredSequences idSeq ""
      | _ -> Format.eprintf "[lreplay warning] invalid row (2 fields expected) at %s:%d@." (List.fold_left (fun s t -> s ^ t) "" fields) linenum; raise (Invalid_argument "")
    with Invalid_argument _ ->
      Format.eprintf "[lreplay warning] invalid row at %s:%d@." file linenum
  in
  if Sys.file_exists file then
    Csv.read_file f file


let load_test_driver_coverage file driver =
  let data = ref [] in
  let covered_ones = ref IntSet.empty in
  let f linenum fields =
    try
      match fields with
      | idstr :: statusstr :: tag :: loc :: [] ->
        let id = int_of_string idstr
        and covered = 0 <> int_of_string statusstr in
        if not (IntSet.exists (fun el -> el = id) !covered_ones) && covered then begin
          covered_ones := IntSet.add id !covered_ones;
          let bindings = [] in
          let start = (String.rindex file '_') + 1 in
          let length = (String.rindex file '.') - start in
          let driver = (String.sub file start length) in
          data := {id; covered; tag; loc; driver; bindings; } :: !data;
          saveLabelTest id driver
        end
      | idstr :: statusstr :: tag :: loc :: nbr :: varFirst :: valFirst :: other_bindings ->
        let id = int_of_string idstr
        and covered = 0 <> int_of_string statusstr in
        if covered then begin
          let bindings = make_bindings (varFirst :: valFirst :: other_bindings) file linenum in
          let start = (String.rindex file '_') + 1 in
          let length = (String.rindex file '.') - start in
          let driver = (String.sub file start length) in
          data := {id; covered; tag; loc; driver ; bindings; } :: !data;
          saveLabelTest id driver
        end
      | _ ->
        Format.eprintf "[lreplay warning] invalid row (3 fields expected) at %s:%d@." (List.fold_left (fun s t -> s ^ t) "" fields) linenum; raise (Invalid_argument "")
    with Invalid_argument _ ->
      Format.eprintf "[lreplay warning] invalid row at %s:%d@." file linenum
  in
  if Sys.file_exists file then
    Csv.read_file f file;
  !data

let overhead = ref 0

let individual_coverage ~driver:file ~outdir:tcdir =
  let binary = Filename.concat tcdir (escape_slash (Filename.chop_extension file)) in
  let output_file = binary^".covlabels" in
  let output_file_seq = binary^".covsequences" in
  if not force && newer_than binary file then
    Format.eprintf "[lreplay] skip test driver compilation %s@." binary
  else
    compile_test_driver file binary output_file output_file_seq;
  if not force && newer_than output_file file then
    Format.eprintf "[lreplay] skip test driver execution %s@." binary
  else
    run_test_driver binary output_file;
  load_test_driver_coverage_seq output_file_seq file;
  load_test_driver_coverage output_file file

(** Put into a hash table, to remove id duplicate keep covered rather than reached *)
let simplify individuals =
  let h = Hashtbl.create 100 in
  let reached = ref 0 in
  let add row =
    let id = row.id in
    incr reached;
    if row.bindings = [] then
      begin
        if not (Hashtbl.mem h id) then
          Hashtbl.add h id row
        else if row.covered && let row' = Hashtbl.find h id in not (row'.covered) then
          Hashtbl.replace h id row
      end
    else
      begin
        if not (Hashtbl.mem h id) then
          Hashtbl.add h id row
        else if row.covered && let row' = Hashtbl.find h id in not (row'.covered) then
          Hashtbl.replace h id row
        else
	if row.covered && not (List.exists (fun row' -> (List.for_all2 (fun e1 e2 -> fst e1 = fst e2 && snd e1 = snd e2) row.bindings row'.bindings)) (Hashtbl.find_all h id)) then begin
	  Hashtbl.add h id row;
	  incr overhead;
	end
      end
  in
  let cpt = ref 0 in
  List.iter (fun data ->
      cpt := !cpt + (List.length data);
      List.iter (fun el -> add el) data
    ) individuals;
  if !cpt != 0 then begin
    Format.eprintf "[lreplay] Row parsed : %d@." !cpt;
    Format.eprintf "[lreplay] Simplified : %d@." (Hashtbl.length h);
  end;
  h

let coverage source =
  let dirname = Filename.dirname source in
  let dirname = if dirname = "" then "." else dirname in
  let basename = Filename.basename source in
  let basename_no_ext = Filename.chop_extension basename in
  let vartbl = Hashtbl.create 10 in
  Hashtbl.add vartbl "SOURCE" source;
  Hashtbl.add vartbl "DIRNAME" dirname;
  Hashtbl.add vartbl "BASENAME" basename;
  Hashtbl.add vartbl "BASENAME_NO_EXT" basename_no_ext;
  Hashtbl.add vartbl "MAINFUN" mainfun;
  let driverpatt = StrUtils.apply_template_hashtbl vartbl driverpatt in
  Format.eprintf "[lreplay] compute coverage for %s with tests %s@." source driverpatt;
  let tcdir = Filename.concat dirname ("lreplay_"^basename_no_ext) in
  if not (Sys.file_exists tcdir) then Unix.mkdir tcdir 0o777;
  let individuals = ref [] in
  StrUtils.glob ~sort:true (fun driver ->
      individuals := individual_coverage ~driver ~outdir:tcdir :: !individuals
    ) driverpatt;
  let table = simplify !individuals in
  let oc = open_out (basename_no_ext^"equiv-in.txt") in
  print_htbl_t_l oc (compute_equiv ());
  print_lst_li oc (compute_imply ());
  close_out oc;
  (table,coveredSequences)
