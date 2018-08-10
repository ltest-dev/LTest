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

module StringSet : (Set.S with type elt = string)

type row = {
  id: int;
  covered: bool;
  tag: string;
  loc: string;
  driver: string;
  bindings : (string * string) list;
}

(**
  Detailed report
*)
type detailed_report = row list

(**
  Simplified report
*)
type report = (int, row) Hashtbl.t

(**
  Compute coverage for a particular driver.

  Compile binary to outdir. Run binary. Read raw data from outdir.
*)
val individual_coverage : ?force:bool -> driver:string -> outdir:string -> detailed_report

(**
  Compute coverage for every matching driver and merge result into a simplified
  report (keep a single test witness for each id).
*)
val coverage : ?force:bool -> string -> string -> string -> report * (int, string) Hashtbl.t



val labelstotest : ((string, StringSet.t) Hashtbl.t)
(*val testtolabels : ((string, string) Hashtbl.t) ref*)


val l_folder : string ref
val l_options : string ref
val l_files : string ref
val i_path : string ref
