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
open Hhelpers

exception CoveredException of string

let print_statistics total ?covered_seq covered_h uncoverable_h =
  Format.eprintf "[lreplay] %s\n" "hyperlabel global statistics";
  Format.eprintf        "  total number of hyperlabels       % 8d@." total;
  begin match covered_seq with
    | None -> ()
    | Some cov ->
      Format.eprintf        "  number of covered sequences       % 8d@." cov;
  end;
  Format.eprintf        "  number of covered hyperlabels     % 8d@." covered_h;
  Format.eprintf        "  number of uncoverable hyperlabels % 8d@." uncoverable_h;
  if total > 0 then
    Format.eprintf "  coverage ratio                % 8d%% @." (covered_h*100 / (total-uncoverable_h))


let print_bindings bl = (List.fold_right (fun (variable,value) l -> "(" ^ variable ^ "," ^ value ^ ") " ^ l ) bl "")

let all_labels_covered env labs = List.for_all (fun el -> (Hashtbl.mem env el) && (List.exists (fun row -> row.covered) (Hashtbl.find_all env el)) ) labs

let all_sequences_covered env seqs = List.for_all (fun el -> (Hashtbl.mem env el)) seqs

let get_value var vals = snd (List.find (fun (variable,value) -> variable = var) vals)

let print_bool b = if b then print_string "true " else print_string "false "

let evaluate_litteral (va,t,vb) comb =
  match va,t,vb with
  | (va,"=",vb) -> (get_value va comb) = (get_value vb comb)
  | (va,"!",vb) -> (get_value va comb) <> (get_value vb comb)
  | (va,t,b) -> raise (Invalid_argument "")

let evaluate_condition cond comb = if (List.fold_right (fun el r -> (evaluate_litteral el comb) && r) cond true) then raise (CoveredException (print_bindings comb))

let rec make_lists h next =
  match next with
  | [] -> [[h]]
  | next -> List.map (fun l -> h :: l) (make_combinations next)

and make_combinations ll =
  List.fold_left (fun acc h -> (make_lists h (List.tl ll)) @ acc) [] (List.hd ll)

let remove_uncovered id row = if row.covered then Some row else None

let get_combinations envs labs seqs =
  if (List.length seqs) > 0 then
    raise (CoveredException "");
  let all_labels_occurrences = List.map (Hashtbl.find_all envs) labs in
  let keep_only_covered = List.map (List.filter (fun row -> row.covered)) all_labels_occurrences in
  let combinations = make_combinations keep_only_covered in
  List.map (List.map (fun row -> row.bindings)) combinations

(* INCOMPLET : ne traite pas les bindings des séquences *)
let compute_conjunction_coverage env conj =
  if ((all_labels_covered (fst env) (conj.labels))
      && (all_sequences_covered (snd env) (conj.sequences))) then
    List.iter (fun l -> evaluate_condition conj.condition (List.concat l)) (get_combinations (fst env) conj.labels conj.sequences)
  else ()

(* begin List.iter (fun l -> Format.eprintf "[hreplay] %s @." ("[" ^ (List.fold_right (fun e r -> (print_bindings e) ^ r) l "") ^ "]")) (get_combinations env conj.labels conj.sequences); Format.eprintf "[hreplay] ---- @." end else List.iter (fun l -> Format.eprintf "[hreplay] %s @." ("[" ^ (List.fold_right (fun e r -> (print_bindings e) ^ r) l "") ^ "]")) (get_combinations env conj.labels conj.sequences); Format.eprintf "[hreplay] ---- @."*)

let covered_h = ref 0
let uncoverable_h = ref 0

let compute_coverage_dnf env dnf =
  try
    List.iter (compute_conjunction_coverage env) dnf;
    " - UNKNOWN - [ ] "
  with
    CoveredException m -> incr covered_h;
    " - COVERED - [ " ^ m ^ " ] "

let compute_coverage i env hlab =
  hlab.id <- i+1 ;
  if String.index_opt hlab.status 'i' <> Some(0) then
    hlab.status <- (compute_coverage_dnf env hlab.ls)
  else begin
    let new_status = compute_coverage_dnf env hlab.ls in
    if StrUtils.contains new_status "COVERED" then begin
      Format.eprintf "[hreplay] warning : Hyperlabels %d previously uncoverable found covered" hlab.id;
      hlab.status <- new_status
    end
    else
      (hlab.status <- " - UNCOVERABLE - [ ] ";incr uncoverable_h);

  end;
  Hhelpers.string_of_node hlab

let stats hfile =
  if Sys.file_exists hfile then  begin
    Format.eprintf "[hreplay] hyperlabel file %s found. Measuring coverage @." hfile;
    try
      let lexbuf = Lexing.from_channel (open_in hfile) in
      let result = Parser.hyplist_root Lexer.read lexbuf in
      List.iter (fun n ->
          if String.index_opt n.status 'c' = Some(0) then incr covered_h;
          if String.index_opt n.status 'i' = Some(0) then incr uncoverable_h
        ) result;
      print_statistics (List.length result) !covered_h !uncoverable_h
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
      print_statistics (List.length result) ~covered_seq:(Hashtbl.length (snd environments)) !covered_h !uncoverable_h
    with Hhelpers.SyntaxError m -> Format.eprintf "[hreplay] %s@." m
  end
  else Format.eprintf "[hreplay] hyperlabel file %s NOT found. Hyp. coverage not measured @." hfile
