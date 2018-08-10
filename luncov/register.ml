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

let add_label_support prj =
  let hfile = Options.Share.file "labels.h" in
  Project.on prj Kernel.CppExtraArgs.append_before ["-include "^hfile];
  ()

let do_run () =
  let t0 = Unix.gettimeofday () in

  (* add header for labels *)
  add_label_support (Project.current ());

  (* initialize labels data and get labels filename *)
  let data = Data.create () in
  let labelsfile =
    if Options.LabelsFile.is_set () then
      Options.LabelsFile.get ()
    else
      let file = List.hd (Kernel.Files.get ()) in
      (Filename.chop_suffix file ".c")^".labels"
  in

  (* load label data if any *)
  let before_stats =
    if Sys.file_exists labelsfile then
      (Data.load data labelsfile; Some (Data.get_stats data))
    else
      None
  in

  let force = Options.Force.get () in

  (* handle the actions *)
  if Options.Init.get () || Options.ForceInit.get () then
    Init.compute ~force:(Options.ForceInit.get ()) data;

  if Options.Value.get () then
    Value.compute ~force data;

  let nb_processes = Options.Multicore.get () in
  let timeout = Options.WpTimeout.get () in
  let kill_timeout = Options.WpKillTimeout.get ()  in
  let max_memory = Options.WpMaxMemory.get () in
  let max_l_per_calls = Options.WPMaxNbLabelPerCall.get () in
  let qed_optim = Options.WPQedOptim.get () in
  let make_proofs = Options.WPMakeProofs.get () in
  let show_logs = Options.WPShowLogs.get() in
  let backup = Options.Backup.get () in
  if Options.WP.get () then (
    if nb_processes < 1 then Wp_singlecore.compute ~force data (truncate (float_of_string timeout))
    else Wp_multicore.compute_multicore ~force data nb_processes timeout kill_timeout max_memory max_l_per_calls qed_optim make_proofs show_logs );

  if Options.VWAP.get () then
    Wp_singlecore.compute ~force data 5;

  begin
    match before_stats with
    | None ->
      Options.feedback "label statistics@.%a" Data.pp_stats (Data.get_stats data)
    | Some before_stats ->
      Options.feedback "label statistics@.%a" Data.pp_diff_stats (before_stats, Data.get_stats data)
  end;

  (* Black magic *)
  if backup then Options.Backup.set true else Options.Backup.set false;

  (* store label data *)
  Data.store data labelsfile;
  Options.feedback "label data written to %s" labelsfile;

  (* Store always true labels *)
  if Options.AlwaysTrue.get () then begin
    let dataAT = Data.create () in
    let labelsfileAT = "AT" ^ labelsfile in
    (if nb_processes < 1 then Wp_singlecore.computeAT ~force dataAT (truncate (float_of_string timeout))
     else Wp_multicore.compute_multicoreAT ~force data nb_processes timeout kill_timeout max_memory max_l_per_calls qed_optim make_proofs show_logs  );
    Data.storeAT dataAT labelsfileAT;
    Options.feedback "always true label data written to %s" labelsfileAT
  end;
  let t1 = Unix.gettimeofday () in
  Options.feedback "Execution time: %f s" (t1-.t0)

let run () =
  if Options.Enabled.get () then
    if Kernel.Files.is_empty () then
      Options.fatal "no input file"
    else
      do_run ()

let () = Db.Main.extend run
