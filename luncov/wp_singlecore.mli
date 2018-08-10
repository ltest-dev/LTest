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

(** Weakest preconditon based uncoverable label detection *)

(** A property checker takes a label identifier and a property that encodes
    its uncoverability; then it updates the property database
*)
type property_checker = string * (label:int -> Property.t -> Property_status.Feedback.t)

(** WP property checker: alias for WP's wp_compute_ip *)
val wp_property_checker : property_checker

(** Launch the detection *)
val compute : ?force:bool -> ?checker:property_checker -> Data.t -> int -> unit

val computeAT : ?force:bool -> ?checker:property_checker -> Data.t -> int -> unit
