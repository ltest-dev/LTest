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

let eval_status cond stmt =
  let state = Db.Value.get_stmt_state stmt in
  if Db.Value.is_reachable state then
    let va_res = !Db.Value.eval_expr ~with_alarms:CilE.warn_none_mode state cond in
    let va_int = Cvalue.V.project_ival va_res in
    if Ival.is_zero va_int then
      (Options.debug "-> uncoverable @[%a@]\n" Db.Value.pretty va_res; Data.Uncoverable)
    else
      (Options.debug "-> unknown @[%a@]\n" Db.Value.pretty va_res; Data.Unknown)
  else
    (Options.debug "-> unreachable\n"; Data.Uncoverable)

let compute_label_with_info ?(force=false) data id info =
  let open Instrument in
  let loc = info.li_loc
  and tag = info.li_tag
  and cond = info.li_predicate
  and stmt = info.li_annotable in
  let current_loc = location_string loc in
  Data.update data ~tag ~current_loc id;
  Options.debug ~level:2 "label #%d cond: @[%a@], tag: %s, loc: @[%a@]\n"
    id Printer.pp_exp cond tag Printer.pp_location loc;
  let status = eval_status cond stmt in (* eval the condition at the given statement *)
  Options.result "label #%d found '%a' by value\n" id Data.pp_status status;
  Data.update data ~force ~status ~emitter:"VA" id (* update the status in data *)


let compute_label ?(force=false) data id =
  !Db.Value.compute ();
  compute_label_with_info ~force data id (Instrument.get id)


let compute ?(force=false) data =
  !Db.Value.compute ();
  Instrument.iter (compute_label_with_info ~force data)
