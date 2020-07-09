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

open Unix
open Cil_types
open Wp

let number_of_processes = ref 1
let wp_timeout = ref 10.
let wp_kill_timeout = ref 10
let wp_max_memory = ref 1000000
let wp_max_l_per_calls = ref 0
let wp_qed_optim = ref true
let wp_make_proofs = ref true
let wp_show_logs = ref true

(** Process pool: a pool of unix processes that compute label statuses in parallel **)

(* Helper Functions *)
let get_kf_name kf =
  match kf.fundec with
  | Definition (f,_) -> f.svar.vname
  | Declaration (_,f,_,_) -> f.vname

let wait_for_children () =
  let output_file = (string_of_int (Unix.getpid()))^"wfc.tmp" in
  ignore(Sys.command ("pgrep -c -P "^(string_of_int(Unix.getpid()))^" --full \"[alt\\-ergo] <defunct>\" > "^output_file));
  if Sys.file_exists output_file then begin
    let ic = open_in output_file in
    let line = input_line ic in
    close_in ic;
    Sys.remove output_file;
    let nb_children = int_of_string (String.trim line) in
    for i = 1 to nb_children do ignore(i); ignore(Unix.wait()) done
  end

let get_resident_memory_usage pid =
  let output_file = (string_of_int (Unix.getpid()))^"mu.tmp" in
  ignore(Sys.command ("ps -p "^(string_of_int pid)^" --no-headers -o rss > "^output_file));
  let ic = open_in output_file in
  let line = input_line ic in
  close_in ic;
  Sys.remove output_file;
  int_of_string (String.trim line)


let rec n_first_a l n a =
  match (l,n) with
  | (l,0) -> (a,l)
  | (h::t,n) -> let new_a = h::a in let new_n = n-1 in (n_first_a t new_n new_a)
  | ([],_) -> (a,[])

let n_first l n = n_first_a l n [] (* Splits the list l after n elements *)

let rec split_list_size_a l n a =
  match n_first l n with
  | (f,h::t) -> let new_l = h::t in let new_a = f::a in (split_list_size_a new_l n new_a)
  | (f,[]) -> f::a

let split_list_size l n =
  if n > 0 then
    split_list_size_a l n []
  else [l] (* Splits a list l in list chunks of size n *)

let no_output () =
  let devnull = descr_of_out_channel (open_out "/dev/null") in
  dup2 devnull stdout

(* The task of one worker process, with result marshalling of the results to the main process *)
let wp_call kf labels channel =
  (* Adding annotations for labels *)
  let ips = Instrument_multicore.add_annotations_get_ips labels in
  (* Proving annotations with WP *)
  Wp_parameters.Timeout.set (int_of_float !wp_timeout);
  if not !wp_qed_optim then begin
    Wp_parameters.Simpl.set false;
    Wp_parameters.Let.set false;
    Wp_parameters.Prune.set false;
    Wp_parameters.Clean.set false;
    Wp_parameters.Bits.set false
  end;

  List.iter (fun s -> Wp_parameters.Model.add s) ["typed";"int";"float";"var"];
  let flag = ref true in
  begin
    try
      VC.(command (generate_kf kf))
    with e ->
      flag := false;
      Options.feedback "WP was interrupted during  check: aborting computation for these labels";
      raise e
  end;
  (* Gathering annotation statuses and sending to root process *)
  Hashtbl.iter (fun lblid ip ->
      let status =
        (match ip with
         | None -> "invalid"
         | Some ip_v ->
           let value_st = List.for_all VC.is_proved (VC.proof ip_v) in
           (*let value_st = (try (is_valid ip_v) with _ -> false) in*)
           if value_st && !flag then ("valid") else ("invalid")
        )
      in
      let message = ("false"^"\n"^(string_of_int lblid)^"\n"^status^"\n") in
      output_string channel message;
      flush channel
    ) ips

(* The task of one intermediate process, creating and managing worker processes *)
let task tasks channel =
  (* Successive calls are made to WP, handling each a part of the labels from labs: avoids WP to consume to much memory if all labels are handled in one time *)
  List.iter (fun (kf,labels) ->
      match fork () with
      | 0 -> (* Handling time-out or memory overflow properly, notably killing and cleaning alt-ergo grand-children processes *)
        Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> ignore(Sys.command ("pkill --signal 9 --parent "^(string_of_int (getpid ())))); wait_for_children(); exit 0));
        if not !wp_show_logs then no_output();
        (* Calling WP *)
        wp_call kf labels channel;
        (* Exiting properly *)
        wait_for_children (); (* WP doesn't seem to wait correctly for the alt-ergo processes it creates. This is repaired here. *)
        exit 0
      | child_pid ->
        Options.feedback "Starting worker process with pid %d to process %d labels of function %s." child_pid (List.length labels) (get_kf_name kf);
        let timeout = (!wp_kill_timeout) in
        let maxmemory = (!wp_max_memory) in
        let time = ref 0 in
        let exited = ref false in
        (* Wait for child to die (via wait) or kills it if its memory usage overflows or execution time takes too long *)
        while !time <= timeout && not (!exited)  && (get_resident_memory_usage child_pid) < maxmemory do
          Unix.sleep 1;
          let (pid,_) = Unix.waitpid [Unix.WNOHANG] child_pid in
          if pid > 0 then exited := true;
          incr time
        done;
        (* Child died *)
        if !exited then begin
          Options.feedback "Worker process with pid %d ended normally" child_pid; ()
        end else
          begin
            (* Killing child *)
            Options.feedback "Killing worker process with pid %d" child_pid;
            Unix.kill child_pid Sys.sigalrm;
            ignore(Unix.waitpid [] child_pid);
            (* Saving all labels treated by child as unknown *)
            List.iter (fun lblid ->
                let message = ("false"^"\n"^(string_of_int lblid)^"\n"^"invalid"^"\n") in
                output_string channel message;
                flush channel
              ) labels
          end;
    ) tasks;
  (* Termination *)
  let message = ("true"^"\n"^(string_of_int (getpid()))^"\n"^"na"^"\n") in
  output_string channel message;
  flush channel;
  close_out channel

(* The main process creates the intermediate processes through unix fork and pipes *)
let rec initialize_pool (_,checker) schedule = create_children 0 ([],(Hashtbl.create !number_of_processes)) checker schedule
and create_children cnt acc_fd chk sch =
  let parameter = (List.hd sch) in
  flush_all ();
  let (fd_in, fd_out) = pipe () in
  match fork () with
  | 0 -> close fd_in; task parameter (out_channel_of_descr fd_out); exit 0
  | child_pid ->
    close fd_out;
    Options.feedback "Starting supervisor process with pid %d" child_pid;
    let new_cnt = cnt + 1 in
    if new_cnt < !number_of_processes then begin
      let res = (create_children new_cnt acc_fd chk (List.tl sch)) in
      (Hashtbl.add (snd res) fd_in (in_channel_of_descr fd_in));
      (fd_in::(fst res),snd res)
    end
    else begin
      (Hashtbl.add (snd acc_fd) fd_in (in_channel_of_descr fd_in));
      (fd_in::(fst acc_fd),snd acc_fd)
    end

(* The main process receives and saves the label statuses computed by the worker processes transmitted through pipes *)
let rec process_results fds chs force (checker_name,_) data = get_results fds chs 0 force checker_name data
and get_results fds chs cnt force checker_name data = match (Unix.select fds [] [] (-1.0)) with
  | fdl, [], [] -> let process acc fd = (
      let channel = Hashtbl.find chs fd in
      let eof = (input_line channel = "true") in
      let lblid = int_of_string (input_line channel) in
      let status = if (input_line channel = "valid") then Data_labels.Uncoverable else Data_labels.Unknown in
      ignore(Sys.command "pkill --signal 9 --parent 1 --full alt-ergo"); (* Dirty but alt-ergo can create polluting subprocesses orphaned when killing wp because of time-out.  *)
      if eof then
        begin
          close_in channel;
          Options.feedback "Exiting supervisor process with pid %d" lblid;
          ignore(Unix.wait());
          fd::acc
        end
      else begin
        Data_labels.update data ~force ~status ~emitter:checker_name lblid;
        acc
      end
    ) in
    let killed_children = (List.fold_left process [] fdl) in
    let nb_killed_children = cnt + (List.length killed_children) in
    if nb_killed_children < !number_of_processes then get_results (List.filter (fun fd -> not (List.mem fd killed_children)) fds) chs nb_killed_children force checker_name data else ()
  | _, _, _ -> failwith "Unexpected pipe interaction with main process."

let process_in_parallel force checker schedule data =
  let (fds,chs) = initialize_pool checker schedule in
  process_results fds chs force checker data

let wp_property_checker =
  let check ~label:_ _ = ()
  in
  ("WP", check)

(** Task a-priori scheduling: given a set of tasks, provide an a-priori distribution between a set of processes **)
(* head_nary [1;2;3;4;5] 2 = ([1,2],[3,4,5]) *)
let rec head_nary l n = split_head [] l n
and split_head h t n =
  if n>0 then begin
    match t with
    | hd::tl -> split_head (h @ [hd]) tl (n - 1)
    | [] -> (h,t)
  end
  else (h,t)

(* split_in_n [1;2;3;4;5] 2 = [[1,2],[3,4],[5]] *)
let rec split_in_n l n =
  match l with
  | _::_ -> let first = head_nary l n in (fst first)::(split_in_n (snd first) n)
  | [] -> []

let compare_tasks (_,labsl) (_,labsr) =
  let nbl = List.length labsl in
  let nbr = List.length labsr in
  nbr-nbl

let rec invert_odds l =
  match l with
  | [] -> l
  | _::[] -> l
  | f::s::t -> f::(List.rev s)::(invert_odds t)


let compose_with_i_th the_split i =
  List.fold_left (fun acc lt ->
      if (List.length lt) <= i then
        acc else
        (List.nth lt i)::acc
    ) [] the_split

let rec create_up_to_n the_split n acc =
  if n <= 0 then
    acc else
    (create_up_to_n the_split (n-1) ((compose_with_i_th the_split (n-1))::acc))

let split tasks nb_processes =
  let the_split = invert_odds (split_in_n tasks nb_processes) in
  create_up_to_n the_split nb_processes []

let split_tasks_for_fair_sharing_of_nb_lablels tasks nb_process =
  let sorted_tasks = List.sort compare_tasks tasks in
  split sorted_tasks nb_process

let generate_tasks (kfs,kf_lb) max = let tasks = ref [] in
  Instrument_multicore.KFSet.iter (fun kf ->
      List.iter (fun el -> tasks := (kf,el)::!tasks) (split_list_size (Hashtbl.find_all kf_lb (get_kf_name kf)) max)
    ) kfs;
  !tasks

let compute_multicore ?(force=false) data nb_process timeout kill_timeout max_memory max_l_per_calls qed_optim make_proofs show_logs =
  number_of_processes := nb_process;
  wp_timeout := float_of_string timeout;
  wp_kill_timeout := kill_timeout;
  wp_max_memory := max_memory;
  wp_max_l_per_calls := max_l_per_calls;
  wp_qed_optim := qed_optim;
  wp_make_proofs := make_proofs;
  wp_timeout := float_of_string timeout;
  wp_show_logs := show_logs;
  let checker = wp_property_checker in
  if Options.Rte.get () then
    !Db.RteGen.compute ();
  Instrument_multicore.at := true;
  ignore(Instrument_multicore.create_project ());
  let tasks = generate_tasks (Instrument_multicore.get_kf_lbl_partition ()) max_l_per_calls in
  Options.feedback "start weakest-precondition-based detection";
  let nbTasks = (List.length tasks) in
  if nbTasks > 0 then begin
    let nb_processes = min nbTasks !number_of_processes in
    number_of_processes := nb_processes;
    let schedule = split_tasks_for_fair_sharing_of_nb_lablels tasks nb_processes in
    process_in_parallel force checker schedule data end;
  Options.feedback "WP-based detection done"

let compute_multicoreAT ?(force=false) data nb_process timeout kill_timeout max_memory max_l_per_calls qed_optim make_proofs show_logs =
  number_of_processes := nb_process;
  wp_timeout := float_of_string timeout;
  wp_kill_timeout := kill_timeout;
  wp_max_memory := max_memory;
  wp_max_l_per_calls := max_l_per_calls;
  wp_qed_optim := qed_optim;
  wp_make_proofs := make_proofs;
  wp_timeout := float_of_string timeout;
  wp_show_logs := show_logs;
  let checker = wp_property_checker in
  if Options.Rte.get () then
    !Db.RteGen.compute ();
  Instrument_multicore.at := false;
  ignore(Instrument_multicore.create_project ());
  let tasks = generate_tasks (Instrument_multicore.get_kf_lbl_partition ()) max_l_per_calls in
  Options.feedback "start weakest-precondition-based detection (for always true labels)";
  let nbTasks = (List.length tasks) in
  if nbTasks > 0 then begin
    let nb_processes = min nbTasks !number_of_processes in
    number_of_processes := nb_processes;
    let schedule = split_tasks_for_fair_sharing_of_nb_lablels tasks nb_processes in
    process_in_parallel force checker schedule data end;
  Options.feedback "WP-based detection (for always true labels) done"
