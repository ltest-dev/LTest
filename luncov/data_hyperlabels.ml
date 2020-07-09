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

open Hhelpers

exception UnknownException

type id = int

type t = (id, node) Hashtbl.t

let create ?(size=100) () = Hashtbl.create size

(* One label at least is uncoverable *)
let exists_uncoverable_label env labs =
  List.exists (fun l -> Data_labels.is_uncoverable env l) labs

(* One sequence at least is uncoverable *)
let exists_uncoverable_sequence env seqs =
  List.exists (fun s -> List.exists (fun s' -> s = s') env) seqs

let exists_uncoverable_bindings env labs =
  List.exists (fun l -> List.exists (fun bl -> l = bl) env) labs

(* INCOMPLET : ne traite pas les bindings *)
(* Unknown iff there is 0 label/sequence uncoverable in the conjunction *)
let compute_conjunction_uncoverable (lbls,seqs,binds) conj =
  if not (exists_uncoverable_label lbls conj.labels)
  && not (exists_uncoverable_sequence seqs conj.sequences)
  && not (exists_uncoverable_bindings binds conj.labels) then
    raise UnknownException

(* a dnf is uncoverable if all its clauses are uncoverables, unknown otherwise *)
let compute_uncoverable_dnf env dnf =
  try
    List.iter (compute_conjunction_uncoverable env) dnf;
    Uncoverable ""
  with
    UnknownException -> Unknown ""

(* Depending on status (old is not unknown iff force option is used),
   decide if we keep the old or new one *)
let merge_status old_status new_status =
  match old_status,new_status with
  | Covered _, Uncoverable _->
    Options.warning "discrepency detected (covered detected as uncoverable)";
    new_status
  | Covered _, Unknown _ ->
    old_status
  | Uncoverable _, Unknown _->
    Options.warning "loss of precision detected";
    new_status
  | _, _ ->
    new_status

(* For each hyperlabel, get its new status *)
let compute_uncoverable ?(force=false) env hlab =
  let old_status = hlab.status in
  if force || Hhelpers.is_unknown hlab then begin
    let new_status = compute_uncoverable_dnf env hlab.ls in
    hlab.status <- merge_status old_status new_status
  end
  else
    Options.feedback "skip hyperlabel #%d" hlab.id

(* Compute coverage (i.e. if uncoverable or not) for all hyperlabels  *)
let compute_coverage ?(force=false) env hdata =
  Hashtbl.iter (fun _ h -> compute_uncoverable ~force env h) hdata

(* Using parser.mly & lexer.mll, load hyperlabels data from .hyperlabels file*)
let load hdata hfile =
  try
    let input = open_in hfile in
    let lexbuf = Lexing.from_channel (open_in hfile) in
    let result = Parser.hyplist_root Lexer.read lexbuf in
    List.iter (fun h -> Hashtbl.add hdata h.id h) result;
    close_in input
  with
    Hhelpers.SyntaxError m -> Format.eprintf "[LUncov] %s@." m

(* Store hdata into hyperlabels file *)
let store hdata hfile =
  if Options.Backup.get () then Commons.backup hfile;
  Format.printf "[LUncov] updating hyperlabel file with coverage data @.";
  let output = open_out hfile in
  let f id node acc =
    let hl = Hhelpers.string_of_node node in
    (id,hl^"\n") :: acc
  in
  let l = Hashtbl.fold f hdata [] in
  let l = List.sort compare l in
  List.iter (fun (_,line)-> output_string output line) l;
  close_out output

type stats = {
  total: int;
  unknown : int;
  covered : int;
  uncoverable: int;
}

let get_stats hdata =
  let total = ref 0 in
  let unknown = ref 0 in
  let covered = ref 0 in
  let uncoverable = ref 0 in
  let incr_counter = function
    | Unknown _ -> incr unknown
    | Covered _ -> incr covered
    | Uncoverable _ -> incr uncoverable
  in
  Hashtbl.iter (fun _ hli -> incr_counter hli.status; incr total) hdata;
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
  Format.fprintf f "total number of hyperlabels %8d@." stats.total;
  Format.fprintf f "number of covered           %8d@." stats.covered;
  Format.fprintf f "number of uncoverable       %8d@." stats.uncoverable;
  Format.fprintf f "number of unknown           %8d@." stats.unknown;
  let pcov rat =
    Format.fprintf f "coverage ratio              %8.2f%% \
                      (covered over maybe coverable)@."
      (100.0 *. rat)
  in
  Extlib.may pcov (coverage_ratio stats)

let pp_diff_stats f (before,after) =
  let pp_diff f n = if n <> 0 then Format.fprintf f " (%+d)" n in
  Format.fprintf f "total number of hyperlabels %8d%a@." after.total
    pp_diff (after.total-before.total);
  Format.fprintf f "number of covered           %8d%a@." after.covered
    pp_diff (after.covered-before.covered);
  Format.fprintf f "number of uncoverable       %8d%a@." after.uncoverable
    pp_diff (after.uncoverable-before.uncoverable);
  Format.fprintf f "number of unknown           %8d%a@." after.unknown
    pp_diff (after.unknown-before.unknown);
  match coverage_ratio before, coverage_ratio after with
  | Some before_rat, Some after_rat ->
    Format.fprintf f "coverage ratio              %8.2f%% \
                      (was %.2f%%)@."
      (100.0 *. after_rat) (100.0 *. before_rat)
  | None, Some after_rat ->
    Format.fprintf f "coverage ratio              %8.2f%% \
                      (covered over maybe coverable)@."
      (100.0 *. after_rat)
  | _ -> ()
