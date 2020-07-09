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

open Cil_types
open Instrument

let get_label id =
  let value = get id in
  match value with
  | Label linfo -> linfo
  | _ -> assert false

let get_loc id =
  (get_label id).li_loc

let get_tag id =
  (get_label id).li_tag

let get_annotable_stmt id =
  (get_label id).li_annotable

let get_predicate id =
  (get_label id).li_predicate

let get_kf_id id =
  (get_label id).li_kf_id

let at = ref Req

(** Visitor that removes all but a single label (given its id),
    add a single assertion whose validity implies the uncoverability of a label,
    and provides information to get a Property.t
*)
class label_selector lblid info prj = object (self)
  inherit Visitor.frama_c_copy prj

  val mutable to_remove = []
  val mutable removed = []

  method private clean_labels stmtl =
    List.filter (fun stmt ->
         if (List.exists (fun sid -> sid = stmt.sid) to_remove) then
           (removed <- stmt :: removed;false)
         else
           true
      ) stmtl

  method private clean_slocals slocals =
    List.filter (fun vi ->
        not (List.exists (fun stmt ->
            match stmt.skind with
            | Block b ->
              Ast_info.is_block_local vi b
            | _ -> assert false
          ) removed)
      ) slocals

  method! vfunc _ =
    Cil.DoChildrenPost (fun f ->
        f.slocals <- self#clean_slocals f.slocals;
        File.must_recompute_cfg f;
        to_remove <- [];
        removed <- [];
        f
      )

  method! vblock _ =
    Cil.DoChildrenPost (fun b ->
        b.bstmts <- self#clean_labels b.bstmts;
        b
      )

  method! vstmt_aux stmt =
    match is_stmt_by_sid stmt.sid with
    | [] -> Cil.DoChildren
    | [lblid'] when lblid' = lblid ->
      self#add_label_annot ();
      Cil.JustCopy
    | _ ->
      to_remove <- stmt.sid :: to_remove;
      Cil.JustCopy

  method private add_label_annot () =
    let lblinfo = get_label lblid in
    let cond = Visitor.visitFramacExpr (self :> Visitor.frama_c_visitor) lblinfo.li_predicate in
    let cond = (Logic_utils.expr_to_term ~coerce:true cond) in
    let assertion = Logic_const.prel (!at, cond, Cil.lzero ()) in (* NB negated*)
    let old_kf = Extlib.the self#current_kf in
    let new_kf = Visitor_behavior.Get.kernel_function self#behavior old_kf in
    let code_annot = AAssert ([], Check, assertion) in
    let code_annotation = Logic_const.new_code_annotation code_annot in
    let queued_action () =
      Annotations.add_code_annot ~kf:new_kf wp_emitter lblinfo.li_annotable code_annotation;
      let ip = Property.ip_of_code_annot_single new_kf lblinfo.li_annotable code_annotation in
      info := Some ip;
    in
    Queue.add queued_action self#get_filling_actions

end

let create_project_for_label ?name id =
  let name = match name with None -> "label_"^string_of_int id | Some name -> name in
  let info = ref None in
  let prj = File.create_project_from_visitor name (new label_selector id info) in
  prj, !info
