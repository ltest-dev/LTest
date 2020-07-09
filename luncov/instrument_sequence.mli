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

(** Get the current location of the label *)
val get_len : id -> int

(** Get the tag of the label *)
val get_var : id -> string

(** Get the kernel function that contains the label *)
val get_kf_id : id -> int

val get_sequences: id -> seqs_info

val get_pos : id -> int -> int

val get_sequence: id -> int -> seq_info


(** Create a project by copy that contains at most the given label.
    Also return a property that encodes its negation.
*)
val create_project_for_sequence : ?name:string -> id -> Project.t * Property.t option
