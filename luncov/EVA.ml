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

open Commons
open Instrument

let uncov_seq = ref []
let uncov_binds = ref []

let eval_status_sequence (ids,ips) =
  let status = Property_status.Feedback.get ips in
  match status with
  | Property_status.Feedback.Valid
  | Property_status.Feedback.Valid_under_hyp
  | Property_status.Feedback.Valid_but_dead ->
    Options.result "sequence #%d found uncoverable by EVA\n" ids;
    uncov_seq := ids :: !uncov_seq
  | _ ->
    Options.result "sequence #%d found unknown by EVA\n" ids

let eval_status cond stmt =
  let state = Db.Value.get_stmt_state stmt in
  if Db.Value.is_reachable state then
    let va_res = !Db.Value.eval_expr ~with_alarms:CilE.warn_none_mode state cond in
    let va_int = Cvalue.V.project_ival va_res in
    if Ival.is_zero va_int then
      (Options.debug "-> uncoverable @[%a@]\n" Db.Value.pretty va_res; Data_labels.Uncoverable)
    else
      (Options.debug "-> unknown @[%a@]\n" Db.Value.pretty va_res; Data_labels.Unknown)
  else
    (Options.debug "-> unreachable\n"; Data_labels.Uncoverable)

let eval_bind_exp cond stmt =
  let state = Db.Value.get_stmt_state stmt in
  if Db.Value.is_reachable state then
    let va_res = !Db.Value.eval_expr ~with_alarms:CilE.warn_none_mode state cond in
    let va_int = Cvalue.V.project_ival va_res in
    if Ival.is_zero va_int then
      Some(false)
    else if not (Ival.contains_zero va_int) then
      Some(true)
    else
      None
  else
    assert false

let compute_label_aux ?(force=false) data id loc tag cond stmt =
  let current_loc = location_string loc in
  Data_labels.update data ~tag ~current_loc id;
  Options.debug ~level:2 "label #%d cond: @[%a@], tag: %s, loc: @[%a@]\n"
    id Printer.pp_exp cond tag Printer.pp_location loc;
  let status = eval_status cond stmt in (* eval the condition at the given statement *)
  Options.result "label #%d found '%a' by EVA\n" id Data_labels.pp_status status;
  Data_labels.update data ~force ~status ~emitter:"EVA" id (* update the status in data *)

let compute_label_with_info ?(force=false) data id info =
  let loc = info.li_loc
  and tag = info.li_tag
  and cond = info.li_predicate
  and stmt = info.li_annotable in
  compute_label_aux ~force data id loc tag cond stmt

let compute_bindings_with_info ?(force=false) data id infos =
  let current_binding = Instrument_binding.get_ids_string id in
  Options.feedback "checking labels/bindings %s" current_binding;
  let uncov = ref false in
  let compute_labels l =
    compute_label_aux ~force data l.bi_id l.bi_loc l.bi_tag l.bi_predicate l.bi_annotable;
    if Data_labels.is_uncoverable data l.bi_id then uncov := true
  in
  List.iter compute_labels infos;
  if not !uncov || force then begin
    match infos, (List.hd infos).bi_tag with
    | [b1;b2], "CACC" ->
      if List.length b1.bi_bindings = 1 && List.length b2.bi_bindings = 1 then begin
        let _, var_p_1 = List.hd b1.bi_bindings in
        let _, var_p_2 = List.hd b2.bi_bindings in
        let status_1 = eval_bind_exp var_p_1 b1.bi_annotable in
        let status_2 = eval_bind_exp var_p_2 b2.bi_annotable in
        begin match status_1, status_2 with
          | Some false, Some false
          | Some true, Some true ->
            Options.result "bindings %s found uncoverable by EVA" current_binding;
            uncov_binds := b1.bi_id :: b2.bi_id :: !uncov_binds
          | _ ->
            Options.result "bindings %s found unknown by EVA" current_binding
        end
      end
      else
        Options.warning "Skip binding annot for %s : Too many bindings [CACC]" current_binding
    | [b1;b2], "RACC" ->
      if List.length b1.bi_bindings = List.length b2.bi_bindings then begin
        let _, var_p_1 = List.hd b1.bi_bindings in
        let _, var_p_2 = List.hd b2.bi_bindings in
        let status_1 = eval_bind_exp var_p_1 b1.bi_annotable in
        let status_2 = eval_bind_exp var_p_2 b2.bi_annotable in
        begin match status_1, status_2 with
          | Some false, Some false
          | Some true, Some true ->
            Options.result "bindings %s found uncoverable by EVA" current_binding;
            uncov_binds := b1.bi_id :: b2.bi_id :: !uncov_binds
          | _ ->
            Options.result "bindings %s found unknown by EVA" current_binding
        end
      end
      else
        Options.warning "Skip binding annot for %s : Too many bindings [CACC]" current_binding
    | _ , "CACC"
    | _ , "RACC" -> Options.warning "Skip binding annot for %s: Too many labels [CACC,RACC]" current_binding
    | _ -> Options.warning "Not supported yet"
  end
  else
    Options.feedback "skip bindings %s" current_binding

let compute_aux ?(force=false) data =
  let f id info =
    match info with
    | Instrument.Label info -> compute_label_with_info ~force data id info
    | Instrument.Binding infos -> compute_bindings_with_info ~force data id infos
    | Instrument.Sequence _ -> ()
  in
  Instrument.iter f

let compute ?(force=false) data hdata =
  let old_prj = Project.current () in
  EVA_sequences.instrument_sequences ();
  let t0 = Unix.gettimeofday () in
  !Db.Value.compute ();
  let t1 = Unix.gettimeofday () in
  if Options.Time.get () then
    Options.feedback "Value compute time: %f s" (t1-.t0);
  compute_aux ~force data;
  List.iter eval_status_sequence (List.sort compare !EVA_sequences.seq_ips);
  Options.feedback "number of uncoverable sequences %d@." (List.length !uncov_seq);
  Data_hyperlabels.compute_coverage ~force (data,!uncov_seq,!uncov_binds) hdata;
  Project.set_current old_prj
