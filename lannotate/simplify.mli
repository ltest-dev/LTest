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

type formula = [
  | `TAnd of formula * formula
  | `TOr of formula * formula
  | `TNot of formula
  | `TAtom of int
  | `TTrue
  | `TFalse
]

val simplify : int -> formula -> formula

module type BOOLEAN_CONVERTIBLE = sig
  type t
  type info

  val convert : ?info:info -> t -> int*info*formula
  val convert_back : info:info -> formula -> t
end

module Make (C : BOOLEAN_CONVERTIBLE) : sig
  val simplify : C.t -> C.t
end

val simplify_exp : Cil_types.exp -> Cil_types.exp
