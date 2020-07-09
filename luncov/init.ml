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

(** Initializer of .labels file *)

open Commons

let compute ?(force=false) data =
  if force then Data_labels.clear data;
  let open Instrument in
  let f_lbls id info =
    let tag = info.li_tag in
    let current_loc = location_string info.li_loc in
    Data_labels.update data ~force ~tag ~current_loc ~status:Data_labels.Unknown id
  in
  let f_bind info =
    let tag = info.bi_tag in
    let id = info.bi_id in
    let current_loc = location_string info.bi_loc in
    Data_labels.update data ~force ~tag ~current_loc ~status:Data_labels.Unknown id;
  in
  let f id info =
    match info with
    | Instrument.Label linfo -> f_lbls id linfo
    | Instrument.Binding binfo -> List.iter (fun bind -> f_bind bind) binfo
    | _ -> ()
  in
  Instrument.iter f;
