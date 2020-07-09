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

let get_binding id =
  let value = get id in
  match value with
  | Binding binfo -> binfo
  | _ -> assert false

let extract id bind_id =
  let binds = get_binding id in
  List.find (fun b -> b.bi_id = bind_id) binds

let get_loc id bind =
  (extract id bind).bi_loc

let get_tag id bind =
  (extract id bind).bi_tag

let get_binds id bind =
  (extract id bind).bi_bindings

let get_kf_id id bind =
  (extract id bind).bi_kf_id

let get_annotable_stmt id bind =
  (extract id bind).bi_annotable

let get_predicate id bind =
  (extract id bind).bi_predicate

let is_current_bind id bind =
  let binds = get_binding id in
  List.exists (fun b -> b.bi_id = bind) binds

let is_last_bind id bind =
  let binds = get_binding id in
  let last = List.hd (List.rev binds) in
  last.bi_id = bind

let get_ids id =
  let binds = get_binding id in
  List.map (fun b -> b.bi_id) binds

let get_ids_string id =
  let binds = get_binding id in
  let binds = List.map (fun b -> "#"^string_of_int b.bi_id) binds in
  String.concat "·" binds

let at = ref Req

(** Visitor that removes all but a binding pair (given its id),
    add a single assertion whose validity implies the uncoverability of a binding,
    and provides information to get a Property.t
*)
class binding_selector id lbl_info binding_info prj = object (self)
  inherit Visitor.frama_c_copy prj

  val mutable to_remove_part = []
  val mutable to_remove = []
  val mutable removed = []
  val mutable previous = []

  method private fusion_blocks stmt  =
    match stmt.skind with
    | Block blast ->
      let bstmts' = ref [] in
      let blocals' = ref [] in
      let bstatics' = ref [] in
      let f stmt' =
        match stmt'.skind with
        | Block b' ->
          bstmts' := b'.bstmts @ !bstmts';
          blocals' := b'.blocals @ !blocals';
          bstatics' := b'.bstatics @ !bstatics';
        | _ -> assert false
      in
      List.iter f previous;
      blast.bstmts <- !bstmts' @ blast.bstmts;
      blast.blocals <- !blocals' @ blast.blocals;
      blast.bstatics <- !bstatics' @ blast.bstatics;
    | _ ->
      assert false

  method private clean_slocals slocals =
    List.filter (fun vi ->
        not (List.exists (fun stmt ->
            match stmt.skind with
            | Block b ->
              Ast_info.is_block_local vi b
            | _ -> assert false
          ) removed)
      ) slocals

  method private clean_labels stmtl =
    List.filter (fun stmt ->
         if (List.exists (fun sid -> sid = stmt.sid) to_remove) then
           (removed <- stmt :: removed;false)
         else
           not (List.exists (fun sid -> sid = stmt.sid) to_remove_part)
      ) stmtl

  method! vfunc _ =
    Cil.DoChildrenPost (fun f ->
        f.slocals <- self#clean_slocals f.slocals;
        File.must_recompute_cfg f;
        to_remove_part <- [];
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
    | [lblid'] when is_current_bind id lblid' ->
      Cil.DoChildrenPost (fun new_stmt ->
          if not (is_last_bind id lblid') then begin
            to_remove_part <- new_stmt.sid :: to_remove;
            previous <- new_stmt :: previous;
          end
          else begin
            self#fusion_blocks new_stmt;
            previous <- [];
            self#add_annot ();
          end;
          new_stmt
        )
    | _ ->
      to_remove <- stmt.sid :: to_remove;
      Cil.JustCopy

  method private mk_check_ca pred =
    Logic_const.new_code_annotation (AAssert ([], Check, pred))

  method private get_new_kf () =
    let old_kf = Extlib.the self#current_kf in
    Visitor_behavior.Get.kernel_function self#behavior old_kf

  method private mk_label_annot exp =
    let cond = Visitor.visitFramacExpr (self :> Visitor.frama_c_visitor) exp in
    let cond = Logic_utils.expr_to_term ~coerce:true cond in
    let assertion = Logic_const.prel (!at, cond, Cil.lzero ()) in
    self#mk_check_ca assertion

  method private mk_ip ca kf stmt =
    Annotations.add_code_annot ~kf wp_emitter stmt ca;
    Property.ip_of_code_annot_single kf stmt ca

  method private mk_bind_imp bi_cond var_pred =
    let cond = Visitor.visitFramacExpr (self :> Visitor.frama_c_visitor) bi_cond in
    let pred = Visitor.visitFramacExpr (self :> Visitor.frama_c_visitor) var_pred in
    let cond = Logic_utils.expr_to_predicate cond in
    let pred = Logic_utils.expr_to_predicate pred in
    let implies = Logic_const.pimplies (cond,pred) in
    let implies_not = Logic_const.pimplies (cond,Logic_const.pnot pred) in
    implies,implies_not

  method private add_labels_annot () =
    let binfos = get_binding id in
    let new_kf = self#get_new_kf () in
    let cas =
      List.map (fun b -> self#mk_label_annot b.bi_predicate) binfos
    in
    let queued_action () =
      let ips =
        List.map2 (fun ca b ->
            b.bi_id, self#mk_ip ca new_kf b.bi_annotable) cas binfos
      in
      lbl_info := ips
    in
    Queue.add queued_action self#get_filling_actions

  method private add_binding_CACC_annot () =
    let new_kf = self#get_new_kf () in
    let binfos = get_binding id in
    match binfos with
    | [b1;b2] ->
      if List.length b1.bi_bindings = 1 && List.length b2.bi_bindings = 1 then begin
        let _, var_p_1 = List.hd b1.bi_bindings in
        let _, var_p_2 = List.hd b2.bi_bindings in
        let imp_1, imp_not_1 = self#mk_bind_imp b1.bi_predicate var_p_1 in
        let imp_2, imp_not_2 = self#mk_bind_imp b2.bi_predicate var_p_2 in
        let imp = Logic_const.pand (imp_1,imp_2) in
        let imp_not = Logic_const.pand (imp_not_1,imp_not_2) in
        let ca_imp = self#mk_check_ca imp in
        let ca_imp_not = self#mk_check_ca imp_not in

        let queued_action () =
          let ip_1 = self#mk_ip ca_imp new_kf b2.bi_annotable in
          let ip_2 = self#mk_ip ca_imp_not new_kf b2.bi_annotable in
          binding_info := ip_1 :: ip_2 :: !binding_info
        in
        Queue.add queued_action self#get_filling_actions
      end
      else
        Options.warning "Skip binding annot for %s : Too many bindings [CACC]" (get_ids_string b1.bi_id)
    | _ ->
      Options.warning "Skip binding annot : Wrong number of bindings [CACC]"

  method private add_binding_RACC_annot () =
    let new_kf = self#get_new_kf () in
    let binfos = get_binding id in
    match binfos with
    | [b1;b2] ->
      if List.length b1.bi_bindings = List.length b2.bi_bindings then begin
        let make_ca (_,var1) (_,var2) =
          let imp_1, imp_not_1 = self#mk_bind_imp b1.bi_predicate var1 in
          let imp_2, imp_not_2 = self#mk_bind_imp b2.bi_predicate var2 in
          let imp_1 = Logic_const.pand (imp_1,imp_not_2) in
          let imp_2 = Logic_const.pand (imp_not_1,imp_2) in
          self#mk_check_ca imp_1, self#mk_check_ca imp_2
        in
        let all_ca = List.map2 make_ca b1.bi_bindings b2.bi_bindings in
        let queued_action () =
          let create_ips (ca_imp_1,ca_imp_2) =
            let ip_1 = self#mk_ip ca_imp_1 new_kf b2.bi_annotable in
            let ip_2 = self#mk_ip ca_imp_2 new_kf b2.bi_annotable in
            binding_info := ip_1 :: ip_2 :: !binding_info
          in
          List.iter create_ips all_ca

        in
        Queue.add queued_action self#get_filling_actions
      end
      else
        Options.warning "Skip binding annot for %s : Wrong number of bindings [RACC]" (get_ids_string b1.bi_id)
    | _ ->
      Options.warning "Skip binding annot : Wrong number of bindings [RACC]"

  method private add_binding_annot () =
    let tag = get_tag id id in
    match tag with
    | "CACC" -> self#add_binding_CACC_annot ()
    | "RACC" -> self#add_binding_RACC_annot ()
    | _ ->
        Options.warning "Skip : Criteria %s not supported yet" tag


  method private add_annot () =
    self#add_labels_annot ();
    self#add_binding_annot ()

end


let create_project_for_binding ?name id =
  let name =
    match name with
    |  None -> "binding_" ^ get_ids_string id
    | Some name -> name
  in
  let lbl_info = ref [] in
  let binding_info = ref [] in
  let prj = File.create_project_from_visitor name (new binding_selector id lbl_info binding_info) in
  let selection = Parameter_state.get_selection_context () in
  Project.copy ~selection prj;
  prj, !lbl_info, List.rev !binding_info
