(**************************************************************************)
(*                                                                        *)
(*  This file is part of LReplay.                                         *)
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

type node_elt = {
  labels: int list;
  sequences : int list;
  condition : (string * string * string) list
}

type node = {
  mutable id: int;
  ls: node_elt list;
  mutable status: string
}

val string_of_node : node -> string

exception SyntaxError of string

val annotateHLabel : int -> node -> string -> node

val createDnfFromLabel : int -> node

val createDnfFromSequence : int -> node

val createDnfFromGuard : node -> (string * string * string) list -> node

val createDnfFromConjunction : node -> node -> node

val createDnfFromDisjunction : node -> node -> node
