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

type status = Unknown | Covered | Uncoverable | Unreachable
type id = int
type label = {
  mutable status: status;
  mutable tag: string;
  mutable origin_loc: string;
  mutable current_loc : string;
  mutable extra : string list;
}
type t = (id, label) Hashtbl.t

val string_of_status : status -> string
val pp_status : Format.formatter -> status -> unit

val create: ?size:int -> unit -> t

val load: ?replace:bool -> t -> string -> unit
val store: string -> t -> unit

val size : t -> int

val get : t -> id -> label
val get_status : t -> id -> status
val set_status : t -> id -> status -> unit

val add : t -> id -> ?tag:string -> ?origin_loc:string -> ?current_loc:string -> ?extra:string list -> status -> unit
val update : t -> id -> ?tag:string -> ?origin_loc:string -> ?current_loc:string -> ?extra:string list -> status -> unit

val iter : (id -> label -> unit) -> t -> unit
