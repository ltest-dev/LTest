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

(** Instrumentation

    Provides access to the information about the label instrumentation in the code
    (location, tag, as well as instrumentation details such as statements).
*)

type id = int

val at : Cil_types.relation ref

type info = {
  li_loc: Cil_types.location;
  li_tag: string;
  li_stmt: Cil_types.stmt;
  li_annotable: Cil_types.stmt;
  li_predicate : Cil_types.exp;
  li_kf: Cil_types.kernel_function;
}

(** Get the current location of the label *)
val get_loc : id -> Cil_types.location

(** Get the tag of the label *)
val get_tag : id -> string

(** Get the statement containing the whole label (i.e. the block) *)
val get_stmt : id -> Cil_types.stmt

(** Get a statement that can be annotated to check the label predicate (currently a special call) *)
val get_annotable_stmt : id -> Cil_types.stmt

(** Get the predicate (currently a C expression, could change in the futre) *)
val get_predicate : id -> Cil_types.exp

(** Get the kernel function that contains the label *)
val get_kf : id -> Cil_types.kernel_function

(** Get all infos about id *)
val get : id -> info

(** Iterate over all labels and their info *)
val iter : (id -> info -> unit) -> unit

val is_annotable_stmt_by_sid : int -> id option
val is_stmt_by_sid : int -> id option
val is_annotable_stmt : Cil_types.stmt -> id option
val is_stmt : Cil_types.stmt -> id option


(** Create a project by copy that contains at most the given label.
    Also return a property that encodes its negation.
*)
val create_project_for_label : ?name:string -> id -> Project.t * Property.t option
