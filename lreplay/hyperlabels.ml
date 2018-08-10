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
open Hhelpers

exception CoveredException of string

let print_statistics total covered =
  Format.eprintf "[lreplay] %s\n" "hyperlabel global statistics";
  Format.eprintf        "  total number of hyperlabels   % 8d@." total;
  Format.eprintf        "  number of covered hyperlabels % 8d@." covered;
  if total > 0 then
    Format.eprintf "  coverage ratio                % 8d%% @." (covered*100 / total)


let print_bindings bl = (List.fold_right (fun (variable,value) l -> "(" ^ variable ^ "," ^ value ^ ") " ^ l ) bl "")

let all_labels_covered env labs = List.for_all (fun el -> (Hashtbl.mem env el) && (List.exists (fun row -> row.covered) (Hashtbl.find_all env el)) ) labs

let all_sequences_covered env seqs = List.for_all (fun el -> (Hashtbl.mem env el)) seqs

let get_value var vals = snd (List.find (fun (variable,value) -> variable = var) vals)

let print_bool b = if b then print_string "true " else print_string "false "

let evaluate_litteral (va,t,vb) comb =
  match va,t,vb with
  | (va,"=",vb) -> (get_value va comb) = (get_value vb comb)
  | (va,"!",vb) -> (get_value va comb) != (get_value vb comb)
  | (va,t,b) -> raise (Invalid_argument "")

let evaluate_condition cond comb = if (List.fold_right (fun el r -> (evaluate_litteral el comb) && r) cond true) then raise (CoveredException (print_bindings comb))

let rec make_lists h next =
  match next with
  | [] -> [[h]]
  | next -> List.map (fun l -> h :: l) (make_combinations next)

and make_combinations ll = List.fold_left (fun o h -> (make_lists h (List.tl ll)) @ o) [] (List.hd ll)

let remove_uncovered id row = if row.covered then Some row else None

let get_combinations envs labs seqs =
  if (List.length seqs) > 0 then
    raise (CoveredException "");
  List.map (List.map (fun row -> row.bindings)) (make_combinations (List.map (List.filter (fun row -> row.covered)) (List.map (Hashtbl.find_all envs) labs)))

(* INCOMPLET : ne traite pas les bindings des séquences ni les mélanges séquence/labels *)
let compute_conjunction_coverage env conj =
  if ((all_labels_covered (fst env) (conj.labels))
      && (all_sequences_covered (snd env) (conj.sequences))) then
    List.iter (fun l -> evaluate_condition conj.condition (List.concat l)) (get_combinations (fst env) conj.labels conj.sequences)
  else ()

(* begin List.iter (fun l -> Format.eprintf "[hreplay] %s @." ("[" ^ (List.fold_right (fun e r -> (print_bindings e) ^ r) l "") ^ "]")) (get_combinations env conj.labels conj.sequences); Format.eprintf "[hreplay] ---- @." end else List.iter (fun l -> Format.eprintf "[hreplay] %s @." ("[" ^ (List.fold_right (fun e r -> (print_bindings e) ^ r) l "") ^ "]")) (get_combinations env conj.labels conj.sequences); Format.eprintf "[hreplay] ---- @."*)

let covered_h = ref 0

let compute_coverage_dnf env dnf =
  try
    List.iter (compute_conjunction_coverage env) dnf;
    " - UNCOVERED - [ ] "
  with
    CoveredException m -> incr covered_h;
    " - COVERED - [ " ^ m ^ " ] "

let compute_coverage i env hlab =
  hlab.id <- i+1 ;
  hlab.status <- (compute_coverage_dnf env hlab.ls);
  Hhelpers.string_of_node hlab

let stats hfile =
  if Sys.file_exists hfile then  begin
    Format.eprintf "[hreplay] hyperlabel file %s found. Measuring coverage @." hfile;
    try
      let lexbuf = Lexing.from_channel (open_in hfile) in
      let result = Parser.hyplist_root Lexer.read lexbuf in
      List.iter (fun n ->
          let i = String.index_opt n.status 'c' in
          if i = Some(0) then incr covered_h
        ) result;
      print_statistics (List.length result) !covered_h
    with Hhelpers.SyntaxError m -> Format.eprintf "[hreplay] %s@." m
  end
  else Format.eprintf "[hreplay] hyperlabel file %s NOT found. Hyp. coverage not measured @." hfile

let hcoverage environments hfile =
  if (Sys.file_exists hfile) then begin
    Format.eprintf "[hreplay] hyperlabel file %s found. Measuring coverage @." hfile;
    try
      let lexbuf = Lexing.from_channel (open_in hfile) in
      let result = Parser.hyplist_root Lexer.read lexbuf in
      let newFile = open_out hfile in
      List.iter (Printf.fprintf newFile "%s\n") (List.mapi (fun i h -> compute_coverage i environments h) result);
      Format.eprintf "[hreplay] updating hyperlabel file with coverage data @.";
      close_out newFile;
      print_statistics (List.length result) !covered_h
    with Hhelpers.SyntaxError m -> Format.eprintf "[hreplay] %s@." m
  end
  else Format.eprintf "[hreplay] hyperlabel file %s NOT found. Hyp. coverage not measured @." hfile
