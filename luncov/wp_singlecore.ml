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

open Commons
open Wp

type property_checker = string * (label:int -> Property.t -> Property_status.Feedback.t)
let wp_timeout = ref 10

(* let wp_compute_ip = Dynamic.get ~plugin:"Wp" "wp_compute_ip" (Datatype.func Property.ty Datatype.unit)
let wp_compute_kf = Dynamic.get ~plugin:"Wp" "wp_compute_kf" (Datatype.func (Datatype.option Kernel_function.ty) (Datatype.func (Datatype.list Datatype.string) (Datatype.func (Datatype.list Datatype.string) Datatype.unit))) *)

let wp_property_checker =
  let check ~label:_ ip =
    let flag = ref 0 in
    Wp_parameters.Timeout.set !wp_timeout;
    Wp_parameters.Assert_check_only.set true;
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

let check_label force (checker_name,checker) data lblid =
  let t0 = Unix.gettimeofday () in
  Options.feedback ~level:2 "checking label #%d" lblid;
  let old = Project.current () in
  let prj, ip = Instrument.create_project_for_label lblid in
  copy_parameters ~src:old prj;
  match ip with
  | None ->
    Options.error "cannot find label #%d" lblid
  | Some ip ->
    let ip_status = with_project prj (fun () -> checker ~label:lblid ip) () in
    let status =
      match ip_status with
      | Property_status.Feedback.Valid | Property_status.Feedback.Valid_under_hyp -> Data.Uncoverable
      | _ -> Data.Unknown
    in
    let exec_time = (Unix.gettimeofday ()) -. t0 in
    Options.feedback "label #%d found '%a' by wp" lblid Data.pp_status status;
    Data.update data ~force ~status ~emitter:checker_name ~exec_time lblid;
    Project.remove ~project:prj ()

let check_label force checker data lblid =
  if force || Data.is_unknown data lblid then
    suceed_or_dump_ast (check_label force checker data) lblid
  else
    Options.feedback "skip label #%d" lblid

let compute ?(force=false) ?(checker=wp_property_checker) data timeout =
  wp_timeout := timeout;
  if Options.Rte.get () then
    !Db.RteGen.compute ();
  Options.feedback "start weakest-precondition-based detection";
  Instrument.at := Cil_types.Req;
  Instrument.iter (fun id _ -> check_label force checker data id);
  Options.feedback "WP-based detection done"


let computeAT ?(force=false) ?(checker=wp_property_checker) data timeout =
  wp_timeout := timeout;
  if Options.Rte.get () then
    !Db.RteGen.compute ();
  Options.feedback "start weakest-precondition-based detection (for always true labels)";
  Instrument.at := Cil_types.Rneq;
  Instrument.iter (fun id _ -> check_label force checker data id);
  Options.feedback "WP-based detection (for always true labels) done"
