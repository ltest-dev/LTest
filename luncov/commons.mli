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


(** Get the fake label function name *)
val label_function_name : string
(** Get the fake sequence function name *)
val seq_cond_function_name: string
(** Get the fake sequence condition function name *)
val seq_function_name: string
(** Get the fake binding function name for 1 binding *)
val bind_function_name: string

(** Extract string constant from Cil expression *)
val cil_isString : Cil_types.exp -> string option

(** Make a simple location string "file:line" from a Cil location *)
val location_string : Cil_types.location -> string

(** Return the last element of a list **)
val last_element : 'a list -> 'a option

(**
    Dump AST (default: current project's AST)
    into a file (default: ".luncov_dump_<project>.c")
*)
val dump_ast: ?file:Cil_types.file -> ?name:string -> unit -> unit

(** Dump AST in case of exception (exception is catched and re-raised) *)
val suceed_or_dump_ast : ?file:Cil_types.file -> ?name:string -> ('a -> 'b) -> 'a -> 'b

(** Apply a function to some argument with a particular Frama-C project.

    The current project is saved and set back when the function returns.
    Like {!Project.on}, but restore the original project even in the function fails.
*)
val with_project : ?selection:State_selection.t -> Project.t -> ('a -> 'b) -> 'a -> 'b

(** Copy command line parameters from one project (default: current one) to another *)
val copy_parameters : ?src:Project.t -> Project.t -> unit

(** Create a predicate that represents the possible values
    of the given expression at the given statement according to EVA

    requires: EVA is computed first and exp is integral
*)
val exp_to_pred: at:Cil_types.stmt -> Cil_types.exp
  -> Cil_types.predicate option

(** Collect all variables (as expressions) directly used by the statement. *)
val collect_variables : Cil_types.stmt -> Cil_datatype.ExpStructEq.Set.t

(** Collect all variables (as expressions) located in the given function **)
val collect_fun_param : Cil_types.fundec -> Cil_datatype.ExpStructEq.Set.t

(** Return either or not the stmt is a label fun call **)
val is_label_stmt : Cil_types.stmt -> int -> (bool * bool)

val backup: string -> unit

val replace_or_add_list: ('a, 'b list) Hashtbl.t -> 'a -> 'b -> unit
val starting_time: float ref
