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

(**
   Partitions the domain of some l-value (as a function) given its type

   The l-value is specified through a function that must returns a fresh copy
   each time (with unique expression ids).

   [depth] is the maximal depth to go into the l-value and [width] is the maximal
   width for array and structures explorations.
*)
val partition_lval : depth:int -> width:int -> emit:(exp -> unit) -> typ -> (unit -> lval) -> unit

(**
   Partitions the domain of some expression (as a function) given its type.

   See {!partition_lval} for details.
*)
val partition_exp : depth:int -> width:int -> emit:(exp -> unit) -> typ -> (unit -> exp) -> unit

module Partition : Annotators.S
