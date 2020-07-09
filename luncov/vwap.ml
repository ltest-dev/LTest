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

open Commons
open Cil_types

(** Define the kind of strategy to apply when performing vwap analysis **)
type strategy = ANNOT_PARAM | ANNOT_LABEL | ANNOT_FUN | ANNOT_ALL | ANNOT_NONE | ANNOT_LABPARAM

(* For assume insertion *)
let luncov_vwap_emitter = Emitter.create "LuncovVWAP"
    [ Emitter.Code_annot ] ~correctness:[] ~tuning:[]

let print_label_status f data =
  let labels = Data_labels.get_label_ids data in
  List.iter (fun i ->
      let l = Data_labels.get_status data i in
      Format.fprintf f "label #%d: %a" i Data_labels.pp_status l
    ) labels

let to_strategy s =
  match s with
  | "none" -> ANNOT_NONE
  | "all" -> ANNOT_ALL
  | "function" -> ANNOT_FUN
  | "param" -> ANNOT_PARAM
  | "label" ->ANNOT_LABEL
  | "label+param" -> ANNOT_LABPARAM
  | _ -> assert false

class annot_collector s id annots = object(self)
  inherit Visitor.frama_c_inplace

  val label_kf_id = Instrument_label.get_kf_id id
  val mutable label_kf = Kernel_function.dummy ()
  val mutable in_label = false

  (* precond s \notin {ANNOT_NONE} *)
  method! vfunc fundec =
    let kf = Extlib.the self#current_kf in
    if Kernel_function.get_id kf == label_kf_id then
      begin
        label_kf <- kf;
        if s = ANNOT_PARAM || s = ANNOT_LABPARAM || s = ANNOT_ALL then
          self#collect_param_annotations fundec;
        if s = ANNOT_PARAM then
          Cil.SkipChildren
        else
          Cil.DoChildren
      end
    else
      Cil.SkipChildren

  (* precond s \notin {ANNOT_NONE, ANNOT_PARAM} *)
  method! vstmt_aux stmt =
    match Instrument.is_stmt_by_sid stmt.sid with
    | [lblid] when lblid = id ->
      in_label <- true;
      self#collect_stmt_annotations stmt;
      Cil.ChangeDoChildrenPost (stmt, fun stmt -> in_label <- false; stmt)
    | _ ->
      if s = ANNOT_ALL || s = ANNOT_FUN || (in_label && s = ANNOT_LABEL) then
        self#collect_stmt_annotations stmt;
      Cil.DoChildren

  method private collect_param_annotations fdec =
    let vars = collect_fun_param fdec in
    let first_stmt = List.hd fdec.sbody.bstmts in
    self#collect_value_annotations first_stmt vars

  method private collect_stmt_annotations stmt =
    let vars = collect_variables stmt in
    self#collect_value_annotations stmt vars

  method private collect_value_annotations stmt vars =
    let on_var var =
      match Commons.exp_to_pred stmt var with
      | Some p -> Queue.add (label_kf, stmt, p) annots
      | _ -> ()
    in
    Cil_datatype.ExpStructEq.Set.iter on_var vars

end

let add_future_annot (kf,stmt,pred) =
  Options.debug ~level:2 "add value annotation `%a` at %a"
    Printer.pp_predicate_node pred.pred_content
    Printer.pp_location (Cil_datatype.Stmt.loc stmt);
  let code_annotation = Logic_const.new_code_annotation (AAssert ([], Check, pred)) in
  Annotations.add_code_annot ~kf luncov_vwap_emitter stmt code_annotation;
  let props = Property.ip_of_code_annot kf stmt code_annotation in
  List.iter (fun prop -> Property_status.emit luncov_vwap_emitter ~hyps:[] prop Property_status.True) props


let wvap_property_checker strat valueprj =
  let _, wp_check = Wp_singlecore.wp_property_checker in
  let check ~label ip =
    let new_ast = Ast.get() in
    let annots = Queue.create () in
    let collect_annots () =
      Visitor.visitFramacFile (new annot_collector strat label annots) new_ast
    in
    Commons.with_project valueprj collect_annots ();
    Queue.iter add_future_annot annots;
    wp_check ~label ip
  in
  ("WVAP", check)


let compute ?(force=false) data hdata timeout =
  let strat = to_strategy (Options.Strategy.get()) in

  Options.feedback "start combined detection (aka VWAP)";
  Options.debug "%a" print_label_status data;
  EVA.compute ~force data hdata;
  Options.debug "%a" print_label_status data;
  let prj = Project.current () in
  begin
    match strat with
    | ANNOT_NONE ->
      Wp_singlecore.compute ~force data hdata timeout
    | _ ->
      Wp_singlecore.compute ~force ~checker:(wvap_property_checker strat prj) data hdata timeout
  end;

  Options.debug "%a" print_label_status data;
  Options.feedback "combined detection done"
