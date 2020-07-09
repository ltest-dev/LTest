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
open Wp

type property_checker = string * (label:int -> Property.t -> Property_status.Feedback.t)
let wp_timeout = ref 10

(***********)
(* Checker *)
(***********)
let wp_property_checker =
  let check ~label:_ ip =
    let flag = ref 0 in
    Wp_parameters.Timeout.set !wp_timeout;
    begin
      try
        VC.(command (generate_ip ip))
      with
        _ ->
        Options.feedback "WP crashed during always true check: aborting computation for this label";
        incr flag
    end;
    let status = Property_status.get ip in
    Options.debug ~level:1 "property: %a -> %a" Property.pretty ip Property_status.pretty status;
    if !flag = 0 then Property_status.Feedback.get ip else Property_status.Feedback.Unknown
  in
  ("WP", check)

(******************)
(* Label checking *)
(******************)
let check_label force (checker_name,checker) data lblid =
  let t0 = Unix.gettimeofday () in
  Options.feedback "checking label #%d" lblid;
  let old = Project.current () in
  let prj, ip = Instrument_label.create_project_for_label lblid in
  copy_parameters ~src:old prj;
  match ip with
  | None ->
    Options.error "cannot find label #%d" lblid
  | Some ip ->
    let ip_status = with_project prj (fun () -> checker ~label:lblid ip) () in
    let status =
      match ip_status with
      | Property_status.Feedback.Valid
      | Property_status.Feedback.Valid_under_hyp
      | Property_status.Feedback.Valid_but_dead -> Data_labels.Uncoverable
      | _ -> Data_labels.Unknown
    in
    let exec_time = (Unix.gettimeofday ()) -. t0 in
    Options.feedback "label #%d found '%a' by wp" lblid Data_labels.pp_status status;
    let tag = Instrument_label.get_tag lblid in
    let current_loc = location_string (Instrument_label.get_loc lblid) in
    Data_labels.update data ~force ~status ~tag ~current_loc ~emitter:checker_name ~exec_time lblid;
    Project.remove ~project:prj ()

let check_label force checker data lblid =
  if force || Data_labels.is_unknown data lblid then
    suceed_or_dump_ast (check_label force checker data) lblid
  else
    Options.feedback "skip label #%d" lblid

(*********************)
(* Sequence checking *)
(*********************)
let uncov_seq = ref []
let check_sequence (_,checker) sid =
  Options.feedback "checking sequence #%d" sid;
  let old = Project.current () in
  let prj, ip = Instrument_sequence.create_project_for_sequence sid in
  copy_parameters ~src:old prj;
  match ip with
  | None ->
    Options.feedback "cannot find sequence #%d" sid
  | Some ip ->
    let ip_status = with_project prj (fun () -> checker ~label:sid ip) () in
    (match ip_status with
     | Property_status.Feedback.Valid
     | Property_status.Feedback.Valid_under_hyp
     | Property_status.Feedback.Valid_but_dead ->
       Options.feedback "sequence #%d found uncoverable by wp" sid;
       uncov_seq := sid :: !uncov_seq
     | _ ->
       Options.feedback "sequence #%d found unknown by wp" sid;
    );
    Project.remove ~project:prj ()

let check_sequence checker sid =
  suceed_or_dump_ast (check_sequence checker) sid

(********************)
(* Binding checking *)
(********************)
let uncov_bindings = ref []
let current_binding = ref ""
let eval_status_binds (_,checker) prj id ips =
  Options.feedback "checking bindings %s" !current_binding;
  let exception Uncov in
  try
    let f ip =
      let status = with_project prj (fun () -> checker ~label:(-1) ip) () in
      match status with
      | Property_status.Feedback.Valid
      | Property_status.Feedback.Valid_under_hyp
      | Property_status.Feedback.Valid_but_dead ->
        Options.result "bindings %s found uncoverable by wp" !current_binding;
        raise Uncov
      | _ -> ()
    in
    List.iter f ips;
    Options.result "bindings %s found unknown by wp" !current_binding
  with Uncov -> uncov_bindings := Instrument_binding.get_ids id @ !uncov_bindings

let check_label_binding force (checker_name, checker) prj data id ip lblid =
  let t0 = Unix.gettimeofday () in
  Options.feedback "checking label #%d" lblid;
  let ip_status = with_project prj (fun () -> checker ~label:lblid ip) () in
  let status =
    match ip_status with
    | Property_status.Feedback.Valid
    | Property_status.Feedback.Valid_under_hyp
    | Property_status.Feedback.Valid_but_dead -> Data_labels.Uncoverable
    | _ -> Data_labels.Unknown
  in
  let exec_time = (Unix.gettimeofday ()) -. t0 in
  Options.feedback "label #%d found '%a' by wp" lblid Data_labels.pp_status status;
  let tag = Instrument_binding.get_tag id lblid in
  let current_loc = location_string (Instrument_binding.get_loc id lblid) in
  Data_labels.update data ~force ~status ~tag ~current_loc ~emitter:checker_name ~exec_time lblid

let check_bindings force checker data id =
  Options.feedback "checking labels/bindings %s" !current_binding;
  let old = Project.current () in
  let prj, ips_lbl, ips_bindings = Instrument_binding.create_project_for_binding id in
  copy_parameters ~src:old prj;
  begin match ips_lbl with
    | [] ->
      Options.error "cannot find labels/bindings %s" !current_binding
    | _ ->
      let f (lblid, ip) =
        if not force && (Data_labels.is_uncoverable data lblid)  then
          Options.feedback "skip label #%d" lblid
        else
          check_label_binding force checker prj data id ip lblid
      in
      List.iter f ips_lbl;
      if not force && List.exists (fun (lblid',_) -> Data_labels.is_uncoverable data lblid') ips_lbl then
        Options.feedback "skip bindings %s" !current_binding
      else
        eval_status_binds checker prj id ips_bindings;
      Project.remove ~project:prj ()
  end

let check_bindings force checker data id =
  current_binding := Instrument_binding.get_ids_string id;
  let binds = Instrument_binding.get_binding id in
  if not force && List.for_all (fun b -> Data_labels.is_uncoverable data b.Instrument.bi_id) binds then
    Options.feedback "skip labels/bindings %s" !current_binding
  else
    suceed_or_dump_ast (check_bindings force checker data) id

let pp_aux f (time,progress,progress_aux) =
  Format.fprintf f "# Done : %d, Left : %d #@." !progress (!Instrument.size_table - !progress);
  Format.fprintf f "# Current execution time : %.2fs #@." time;
  Format.fprintf f "# Estimated time left : %.2fs #@." ((time /. progress_aux) *. (100. -. progress_aux))

exception Timeout

let progress_step = 100
let progress = ref 0
let print_progress () =
    let time = Unix.gettimeofday () -. !Commons.starting_time in
    let progress_aux = float_of_int !progress /. (float_of_int !Instrument.size_table)  *. 100. in
    if (int_of_float time) > Options.WpTimeout.get () then begin
      Options.feedback "# Current progress : %.2f%% #@.%a" progress_aux pp_aux (time,progress,progress_aux);
      raise Timeout
    end
    else if !progress mod progress_step = 0 then
      Options.feedback "# Current progress : %.2f%% #@.%a" progress_aux pp_aux (time,progress,progress_aux)

let compute ?(force=false) ?(checker=wp_property_checker) data hdata timeout =
  wp_timeout := timeout;
  if Options.Rte.get () then
    !Db.RteGen.compute ();
  Options.feedback "start weakest-precondition-based detection";
  let seen_seq = ref false in
  let f id info =
    begin match info with
      | Instrument.Label _ -> check_label force checker data id
      | Instrument.Sequence _ -> seen_seq:=true; check_sequence checker id
      | Instrument.Binding _ -> check_bindings force checker data id
    end;
    incr progress;
    print_progress ()
  in
  begin
    try
      Instrument.iter f;
      Options.feedback "WP-based detection done";
    with Timeout ->
      Options.feedback "Timeout reached (-luncov-wp-timeout), stoping WP Analysis)";
      Options.feedback "%d of %d labels/sequences analysed" !progress !Instrument.size_table;
      Options.feedback "WP-based detection interrupted (timeout)";
  end;
  Data_hyperlabels.compute_coverage ~force (data,!uncov_seq,!uncov_bindings) hdata;
  if !seen_seq then
    Options.feedback "number of uncoverable sequences %d@." (List.length !uncov_seq)


let computeAT ?(force=false) ?(checker=wp_property_checker) data timeout =
  wp_timeout := timeout;
  if Options.Rte.get () then
    !Db.RteGen.compute ();
  Options.feedback "start weakest-precondition-based detection (for always true labels)";
  Instrument_label.at := Cil_types.Rneq;
  Instrument.iter_lbls (fun id _ -> check_label force checker data id);
  Options.feedback "WP-based detection (for always true labels) done"
