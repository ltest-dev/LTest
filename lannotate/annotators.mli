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

type annotation =
  int * string * Cil_types.exp * Cil_types.location

type annotator

(** Module type of annotators *)
module type ANNOTATOR = sig
  val name : string
  val help : string

  (** Insert IN PLACE the annotation on the given AST.

      In addition of the AST, it also takes a function as parameter.
      This function is provided by Annotators, it makes a label statement
      from a boolean condition and an "origin" location (e.g. the
      if-then-else condition's location in the case of a MCC label).
  *)
  val apply : (Cil_types.exp -> Cil_types.exp list -> Cil_types.location -> Cil_types.stmt) -> Cil_types.file -> unit
end

module type ANNOTATOR_WITH_EXTRA_TAGS = sig
  val name : string
  val help : string
  val apply : (extra:string list -> Cil_types.exp -> Cil_types.exp list -> Cil_types.location -> Cil_types.stmt) -> Cil_types.file -> unit
end

module type S = sig
  val self : annotator
  val apply : ?id:(unit -> int) -> ?collect:(annotation -> unit) -> Cil_types.file -> unit
end

(**
   Register an annotator
*)
module Register (A : ANNOTATOR) : S

(**
   Register an annotator that provides extra tags (for instance, mutator tags)
   in addition of the annotator name
*)
module RegisterWithExtraTags (A : ANNOTATOR_WITH_EXTRA_TAGS) : S

val get_file_name : unit -> string

val annotate_with : annotator -> ?id:(unit -> int) -> ?collect:(annotation -> unit) -> Cil_types.file -> unit

val annotate : string -> string list -> ?id:(unit->int) -> ?collect:(annotation -> unit) -> Cil_types.file -> unit

val shouldInstrument : Cil_types.varinfo -> bool

val print_help : Format.formatter -> unit

val getCurrentLabelId : unit -> int
val getCurrentBindingId: unit -> int
val getCurrentHLId: unit -> int

val label_function_name : string ref

val next : unit -> int
val next_hl: unit -> string
val next_binding: unit -> int
