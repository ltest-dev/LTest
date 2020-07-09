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

type t

(** Hyperlabel identifier *)
type id = int

(** Create a hyperlabel table *)
val create : ?size:int -> unit -> t

(** load hdata from a file *)
val load : t -> string -> unit

(** Compute coverage for hyperlabels *)
val compute_coverage : ?force:bool -> Data_labels.t * int list * int list -> t -> unit

val store : t -> string -> unit

type stats = {
  total: int;
  unknown : int;
  covered : int;
  uncoverable: int;
}

(** Get stats from data *)
val get_stats : t -> stats

(** Pretty print statistics *)
val pp_stats : Format.formatter -> stats -> unit

(** Pretty print differential statistics, given a pair before-after *)
val pp_diff_stats : Format.formatter -> (stats*stats) -> unit
