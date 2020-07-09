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

type id = int

type lbl_info = {
  li_loc : Cil_types.location;
  li_tag : string;
  li_annotable : Cil_types.stmt;
  li_predicate : Cil_types.exp;
  li_kf_id : int;
}
type seq_info = {
  si_loc: Cil_types.location;
  si_pos : int;
  si_annotable : Cil_types.stmt;
}
type seqs_info = {
  ssi_len : int;
  ssi_var : string;
  ssi_kf_id : int;
  mutable ssi_seqs : seq_info list;
  mutable ssi_conds: int list;
}
type bind_info = {
  bi_id : int;
  bi_loc : Cil_types.location;
  bi_tag : string;
  bi_annotable : Cil_types.stmt;
  bi_predicate : Cil_types.exp;
  bi_kf_id : int;
  bi_bindings : (string*Cil_types.exp) list;
}

type info = Label of lbl_info | Sequence of seqs_info | Binding of bind_info list

val wp_emitter: Emitter.t
val eva_emitter: Emitter.t

(** Get all infos about id *)
val get : id -> info

(** Iterate over all labels and their info *)
val iter : (id -> info -> unit) -> unit
(** Iterate over all labels and their info *)
val iter_lbls : (id -> lbl_info -> unit) -> unit
(** Iterate over all labels and their info *)
val iter_seqs : (id -> seqs_info -> unit) -> unit
(** Iterate over all labels and their info *)
val iter_binds : (id -> bind_info list -> unit) -> unit

val is_annotable_stmt_by_sid : int -> id option
val is_stmt_by_sid : int -> id list
val size_table: int ref
