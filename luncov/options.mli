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
   LUncov parameters and plug-in services
*)

include Plugin.S

(** Enable uncoverable label detection (default: disabled) *)
module Enabled : Parameter_sig.Bool

(** Enable (re-)initialization of label data from source files (default: disabled) *)
module Init : Parameter_sig.Bool

(** Force (re-)initialization of label data from source files (default: disabled) *)
module ForceInit : Parameter_sig.Bool

(** Enable WP-based detection (default: disabled) *)
module WP : Parameter_sig.Bool

(** Enable Value-based detection (default: enabled) *)
module Value : Parameter_sig.Bool

(** Enable WP-based detection of always true labels (default: disabled) *)
module AlwaysTrue : Parameter_sig.Bool

(** Force computation and write (default: disabled)

    For WP, force computation even for label already marked uncoverable
    or covered.

    For WP and Value, the label database is updated even if previous status
    was more precise or divergent (should not be used with both WP and
    Value at the same time).
*)
module Force : Parameter_sig.Bool

(** Enable RTE generation for WP-based detection (default: enabled) *)
module Rte : Parameter_sig.Bool

(** Enable VA + WP analysis (default:disabled)  **)
module VWAP : Parameter_sig.Bool

(** Prints WP logs (default:enabled) **)
module WPShowLogs : Parameter_sig.Bool

(** directory where to get and write labels files **)
module LabelsFile : Parameter_sig.String

(** Strateguy to use for vwap analysis **)
module Strategy : Parameter_sig.String

(** Set number of processes for parallel WP-based detection (default: 1) **)
module Multicore : Parameter_sig.Int

(** Set the timeout (in seconds) for provers used in WP (default: 10) **)
module WpTimeout : Parameter_sig.String

(** Set the maximal redident memory usage (in kB) for each complete WP run **)
module WpMaxMemory : Parameter_sig.Int

(** Set the timeout (in seconds) for each complete WP run **)
module WpKillTimeout : Parameter_sig.Int

(** Set the maximal number of labels per call to WP **)
module WPMaxNbLabelPerCall : Parameter_sig.Int

(** Enable formula optimizations in QED during WP call **)
module WPQedOptim : Parameter_sig.Bool

(** Enable call to provers in WP **)
module WPMakeProofs : Parameter_sig.Bool

(** Backup the .labels file **)
module Backup : Parameter_sig.Bool
