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

(**
   Valabels' computation
*)

(**
 * Compute for a single label
 *
 * N.B.: Value analysis is still run globally
 *)
val compute_label: ?force:bool -> Data.t -> Data.id -> unit

(**
   Compute for each label if it's obviously "uncoverable" (using the value
   analysis) and updates some coverage data (maybe empty initially).

   Requires AST and Value to be computed. Also it expects labels of the form
   __PC__LABEL(id,cond).
*)
val compute: ?force: bool -> Data.t -> unit
