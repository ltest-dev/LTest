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

let add_label_support prj =
  let hfile = Format.asprintf "%a" Filepath.Normalized.pp_abs (Options.Share.get_file ~mode:`Must_exist "labels.h") in
  Project.on prj Kernel.CppExtraArgs.append_before ["-include "^hfile];
  ()

let do_run () =
  (* initialize labels data and get labels filename *)
  let data = Data_labels.create () in
  let file = Filepath.Normalized.to_pretty_string (List.hd (Kernel.Files.get ())) in

  let labelsfile =
    if Options.LabelsFile.is_set () then
      Options.LabelsFile.get ()
    else
      (Filename.chop_suffix file ".c")^".labels"
  in

  (* initialize hyperlabels data and get hyperlabels filename *)
  let hdata = Data_hyperlabels.create () in
  let hyperlabelsfile =
    if Options.HyperlabelsFile.is_set () then
      Options.HyperlabelsFile.get ()
    else
      (Filename.chop_suffix file ".c")^".hyperlabels"
  in

  (* load label data if any *)
  let labels_before_stats =
    if Sys.file_exists labelsfile then
      (Data_labels.load data labelsfile; Some (Data_labels.get_stats data))
    else
      None
  in

  (* load hyperlabel data if any *)
  let hyperlabels_before_stats =
    if Sys.file_exists hyperlabelsfile then begin
      Format.printf "[LUncov] hyperlabel file %s found.@." hyperlabelsfile;
      Data_hyperlabels.load hdata hyperlabelsfile;
      Some (Data_hyperlabels.get_stats hdata)
    end
    else begin
      Format.printf "[LUncov] hyperlabel file %s NOT found. Hyp. coverage not measured @." hyperlabelsfile;
      None
    end
  in

  let force = Options.Force.get () in

  (* handle the actions *)
  if Options.Init.get () || Options.ForceInit.get () then
    Init.compute ~force:(Options.ForceInit.get ()) data;

  if Options.EVA.get () then
    (EVA.compute ~force data hdata);

  let nb_processes = Options.Multicore.get () in
  let timeout = Options.SolverTimeout.get () in
  let kill_timeout = Options.WpKillTimeout.get ()  in
  let max_memory = Options.WpMaxMemory.get () in
  let max_l_per_calls = Options.WPMaxNbLabelPerCall.get () in
  let qed_optim = Options.WPQedOptim.get () in
  let make_proofs = Options.WPMakeProofs.get () in
  let show_logs = Options.WPShowLogs.get() in
  let backup = Options.Backup.get () in

  if Options.WP.get () then (
    if nb_processes <= 1 then
      Wp_singlecore.compute ~force data hdata (truncate (float_of_string timeout))
    else
      Wp_multicore.compute_multicore ~force data nb_processes timeout kill_timeout max_memory max_l_per_calls qed_optim make_proofs show_logs );

  if Options.VWAP.get () then
    Options.warning "WIP : TURNED OFF FOR NOW";
    (* Vwap.compute ~force data hdata 5; *)

  (* Black magic *)
  if backup then Options.Backup.set true else Options.Backup.set false;

  begin
    match labels_before_stats with
    | None ->
      Options.feedback "label statistics@.%a" Data_labels.pp_stats (Data_labels.get_stats data)
    | Some before_stats ->
      Options.feedback "label statistics@.%a" Data_labels.pp_diff_stats (before_stats, Data_labels.get_stats data)
  end;

  begin
    match hyperlabels_before_stats with
    | None -> ()
    | Some before_stats ->
      Options.feedback "hyperlabel statistics@.%a" Data_hyperlabels.pp_diff_stats
        (before_stats, Data_hyperlabels.get_stats hdata);
      (* store hyperlabel data *)
      Data_hyperlabels.store hdata hyperlabelsfile;
      Options.feedback "hyperlabel data written to %s" hyperlabelsfile;
  end;

  (* store label data *)
  Data_labels.store data labelsfile;
  Options.feedback "label data written to %s" labelsfile;

  (* Store always true labels *)
  if Options.AlwaysTrue.get () then begin
    let dataAT = Data_labels.create () in
    let labelsfileAT = "AT" ^ labelsfile in
    (if nb_processes < 1 then Wp_singlecore.computeAT ~force dataAT (truncate (float_of_string timeout))
     else Wp_multicore.compute_multicoreAT ~force data nb_processes timeout kill_timeout max_memory max_l_per_calls qed_optim make_proofs show_logs  );
    Data_labels.storeAT dataAT labelsfileAT;
    Options.feedback "always true label data written to %s" labelsfileAT
  end;
  let t1 = Unix.gettimeofday () in
  if Options.Time.get() then
    Options.feedback "Execution time: %f s" (t1 -. !Commons.starting_time)
  else
    Options.feedback "Execution time: (disabled)"

let run () =
  if Kernel.Files.is_empty () then
    Options.abort "no input file"
  else
    do_run ()

let run_once, _ = State_builder.apply_once "LUncov.run" [] run

let main () =
  if Options.Enabled.get () then run_once ()


let setup_run () =
  if Options.Enabled.get () then begin
    Commons.starting_time := Unix.gettimeofday ();
    Dynamic.Parameter.Bool.off "-variadic-translation" ();
    (* add header for labels *)
    add_label_support (Project.current ());
  end

let () = Cmdline.run_after_configuring_stage setup_run

let () = Db.Main.extend main
