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

(**
   Valabels' computation
*)

(**
 * Compute for a single label
 *
 * N.B.: EVA is still run globally
*)
(* val compute_label: ?force:bool -> Data_labels.t -> Data_labels.id -> unit *)

(**
   Compute for each label if it's obviously "uncoverable" (using EVA) and updates some coverage data (maybe empty initially).

   Requires AST and EVA to be computed. Also it expects labels of the form
   __PC__LABEL(id,cond).
*)
val compute: ?force: bool -> Data_labels.t -> Data_hyperlabels.t -> unit
