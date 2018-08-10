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

open Cil_types

module Exp : sig
  val mk : ?loc:location -> exp_node -> exp

  (** [int] zero *)
  val zero : ?loc:location -> unit -> exp

  (** [int] one *)
  val one : ?loc:location -> unit -> exp

  (** [int] constant *)
  val integer : ?loc:location -> int -> exp

  (** [ikind] constant *)
  val kinteger : ?loc:location -> Cil_types.ikind -> int -> exp

  (** [float] constant *)
  val float : ?loc:location -> float -> exp

  (** [string] constant *)
  val string : ?loc:location -> string -> exp

  val var : ?loc:location -> varinfo -> exp

  val lval : ?loc:location -> lval -> exp

  val mem : ?loc:location -> addr:exp -> off:offset -> exp

  (** Logical not *)
  val lnot : ?loc:location -> exp -> exp

  (** Arithmetic negation *)
  val neg : ?loc:location -> exp -> exp

  (** Binary operation *)
  val binop : ?loc:location -> binop -> exp -> exp -> exp

  (** Implies *)
  val implies : ?loc:location -> exp -> exp -> exp

  (** Iff *)
  val iff : ?loc:location -> exp -> exp -> exp

  (** Xor, boolean disequality *)
  val niff : ?loc:location -> exp -> exp -> exp

  (** Replace some subexpression by another (== equality) *)
  val replace : whole:exp -> part:exp -> repl:exp -> exp

  (** Joins some expressions (at least one) with a binary operator. *)
  val join : ?loc:location -> binop -> exp list -> exp
  val rev_join : ?loc:location -> binop -> exp list -> exp

  val copy : exp -> exp
end

module Lval : sig
  val var : varinfo -> lval
  val mem : addr:exp -> off:offset -> lval
  val addOffset: off:offset -> base:lval -> lval
end

module Stmt : sig
  val mk : ?ghost:bool -> ?valid_sid:bool -> stmtkind -> stmt
  (** Make a block statement from a list of statements. *)
  val block : stmt list -> stmt
end

module Block : sig
  val mk : stmt list -> block
end
