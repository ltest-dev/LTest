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

open Cil_types

module Printer : Printer_api.S

val extract_global_vars : file -> varinfo list

val all_stmts : stmt list ref
val get_stmt_loc : stmt -> location
val get_stmt_loc_int : stmt -> int
val print_file_path : Cil_types.location -> string
val same_line : stmt -> stmt -> bool
val mk_call :
  ?loc:location -> ?result:lval -> string -> exp list -> stmt

val mkdir : string -> unit

(** Indicates whether an instruction is a label. *)
val is_label : instr -> bool

(**
   Indicates whether an expression is boolean in itself.
   Used to detect boolean expression outside conditional statement
*)
val is_boolean: exp -> bool

(**
   Get atomic conditons form a boolean expression.
*)
val atomic_conditions : exp -> exp list

(** [combine n l] computes the combinations of [n] elements from the list [l].

    Returns the combination in the order of the list [l] and in a depth-first
    manner. For instance, [combine 2 [1;2;3]] returns [[1;2];[1;3];[2;3]].
*)
val combine : int -> 'a list -> 'a list list

(**
   [rev_combine n l] computes the combinations of [n] elements
   from the list [l].

   Returns the combination in the opposite order of {!combine}.
*)
val rev_combine : int -> 'a list -> 'a list list

(**
   [sign_combine pos neg l] computes all sign combinations of a list of
   elements [l], given two sign functions [pos] and [neg].

   Preserves the original order of the list, i.e. each sublist is
   in the same order.

   For instance, [sign_combine (fun x ->"+"^x) (fun x -> "-"^x) ["1";"2"]]
   returns [["+1";"+2"];["+1";"-2"];["-1";"+2"];["-1";"-2"]].
*)

val sign_combine : pos:('a -> 'b) -> neg:('a -> 'b) -> 'a list -> 'b list list

(**
   [sign_combine pos neg l] computes all sign combinations of a list of
   elements [l], given two sign functions [pos] and [neg].

   Returns the combination in the opposite order of {!sign_combine}.
*)
val rev_sign_combine : pos:('a -> 'b) -> neg:('a -> 'b) -> 'a list -> 'b list list

val concat: 'a list list -> 'a list
