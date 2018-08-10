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

val apply_template : (string -> string) -> string -> string
val apply_template_hashtbl :
  ?fallback:(string -> string) -> (string,string) Hashtbl.t -> string -> string

(** File name matching *)

type fnmatch
val fnmatch_compile: string -> fnmatch
val fnmatch_match: fnmatch -> string -> bool

(** Run the callback for every file matching the pattern. *)
val glob: ?sort:bool -> (string -> unit) -> string -> unit

(** Get a list of every file matching the pattern. *)
val glob_list: ?sort:bool -> string -> string list


(**
  Escape for shell use, i.e. put between single quotemarks
  (and escape them, ['] becomes ['\'']).
*)
val shell_escape : string -> string


(**
 * [explode s sep] explodes the string [s] into non-empty chunks at each occurrence of a character from sep.
 *)
val explode : string -> string -> string list
