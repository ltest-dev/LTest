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

(** Label data, internally a hash table *)
type t

(** Label status *)
type status =
  | Unknown
  | Covered
  | Uncoverable

(** Label identifier *)
type id = int

(** Print a status *)
val pp_status : Format.formatter -> status -> unit

(** Convert status to string *)
val string_of_status : status -> string

(** Create a label table *)
val create : ?size:int -> unit -> t

(** Get the number of labels in the table *)
val size : t -> int

(** Load a file into table *)
val load : t -> ?replace:bool -> string -> unit

(** Store table to a file *)
val store : t -> string -> unit

val storeAT : t -> string -> unit

(** Update a label status and possibly some other label fields *)
val update : t -> ?force:bool -> ?status:status -> ?tag:string
  -> ?origin_loc:string -> ?current_loc:string -> ?emitter:string
  -> ?exec_time:float -> ?extra:string list -> id -> unit

(** Check whether the label has the status unknown *)
val is_unknown : t -> id -> bool

(** Check whether the label has the status uncoverable *)
val is_uncoverable : t -> id -> bool

(** Return the status associated with a label id **)
val get_status : t -> id -> status

(** Return the emitter of the status **)
val get_emitter : t -> id -> string

(** Return the status associated with a label id **)
val get_exec_time : t -> id -> float

(** Get the list of label ids *)
val get_label_ids : t -> id list

(** Clear all data *)
val clear : t -> unit

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
