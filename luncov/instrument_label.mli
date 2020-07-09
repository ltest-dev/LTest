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

(** Instrumentation

    Provides access to the information about the label instrumentation in the code
    (location, tag, as well as instrumentation details such as statements).
*)

open Instrument

val at : Cil_types.relation ref

(** Get the current location of the label *)
val get_loc : id -> Cil_types.location

(** Get the tag of the label *)
val get_tag : id -> string

(** Get a statement that can be annotated to check the label predicate (currently a special call) *)
val get_annotable_stmt : id -> Cil_types.stmt

(** Get the predicate (currently a C expression, could change in the futre) *)
val get_predicate : id -> Cil_types.exp

(** Get the kernel function that contains the label *)
val get_kf_id : id -> int

val get_label: id -> lbl_info

(** Create a project by copy that contains at most the given label.
    Also return a property that encodes its negation.
*)
val create_project_for_label : ?name:string -> id -> Project.t * Property.t option
