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

let get_sequences id =
  let value = get id in
  match value with
  | Sequence sinfo -> sinfo
  | _ -> assert false

let get_var id =
  (get_sequences id).ssi_var

let get_len id =
  (get_sequences id).ssi_len

let get_kf_id id =
  (get_sequences id).ssi_kf_id

let get_sequence id sid =
  let seqs = (get_sequences id).ssi_seqs in
  List.find (fun s -> s.si_annotable.sid = sid) seqs

let get_pos id sid =
  (get_sequence id sid).si_pos

let is_cond id sid =
  List.exists (fun sid' -> sid' = sid) (get_sequences id).ssi_conds

let init_vinfo ids =
  let vi = Cil.makeVarinfo false true
      ("__SEQ_STATUS_" ^ string_of_int ids)
      (TInt(IInt,[]))
  in
  vi.vghost <- true;
  vi

let unk_loc = Cil_datatype.Location.unknown
let to_add = Hashtbl.create 32

(** Visitor that removes all but a single sequence (given its ids),
    add a single assertion (and some ghost code) whose validity
    implies the uncoverability of a sequence and provides
    information to get a Property.t
*)
class sequence_selector ids sinfo prj = object (self)
  inherit Visitor.frama_c_copy prj

  val sequence_kf_id = get_kf_id ids
  val vInfo = init_vinfo ids
  val inLoop = Stack.create ()
  val seen = ref false
  val mutable to_remove = []

  method private mk_cond ?(loc=unk_loc) value =
    let lval = Cil.new_exp ~loc (Lval (Cil.var vInfo)) in
    let test = Cil.integer ~loc value in
    Cil.mkBinOp ~loc Cil_types.Eq lval test

  method private mk_set ?(loc=unk_loc) value =
    if not (Stack.is_empty inLoop) then seen := true;
    let lval = Cil.var vInfo in
    let new_value = Cil.integer ~loc value in
    let set = Ast_info.mkassign lval new_value loc in
    Cil.mkStmtOneInstr ~ghost:true set

  method private mk_if ?(loc=unk_loc) value =
    let cond = self#mk_cond ~loc (value-1) in
    let set = self#mk_set ~loc value in
    let thenb = Cil.mkBlock [set] in
    let elseb = Cil.mkBlock [] in
    Cil.mkStmt ~ghost:true (If(cond,thenb,elseb,loc))

  method private get_init_stmt () =
    let init = Cil.makeZeroInit unk_loc (TInt(IInt,[])) in
    let local_init = Local_init(vInfo,AssignInit(init),unk_loc) in
    Cil.mkStmtOneInstr ~ghost:true local_init

  method private clean_labels stmtl =
    List.filter (fun stmt ->
        not (List.exists (fun sid ->
            sid = stmt.sid) to_remove)
      ) stmtl

  method! vfunc _ =
    let current_kf_id = Kernel_function.get_id (Extlib.the self#current_kf) in
    Cil.DoChildrenPost (fun f ->
        if sequence_kf_id = current_kf_id then begin
          f.slocals <- f.slocals @ [vInfo];
          f.sbody.blocals <- f.sbody.blocals @ [vInfo];
          f.sbody.bstmts <- (self#mk_set 0)::f.sbody.bstmts;
        end;
        File.must_recompute_cfg f;
        to_remove <- [];
        seen:=false;
        f
      )

  method! vblock _ =
    Cil.DoChildrenPost (fun b ->
        let rec aux = function
          | [] -> []
          | s :: t ->
            if Hashtbl.mem to_add s.sid then
              (List.rev (Hashtbl.find to_add s.sid)) @ [s] @ (aux t)
            else
              s :: aux t
        in
        b.bstmts <- self#clean_labels b.bstmts;
        b.bstmts <- aux b.bstmts;
        b
      )

  method! vstmt_aux stmt =
    let loc = Cil_datatype.Stmt.loc stmt in
    match stmt.skind with
    | Loop _ ->
      Stack.push stmt.sid inLoop;
      Cil.DoChildrenPost ( fun new_stmt ->
          if !seen then begin
            let lv = Cil.cvar_to_lvar vInfo in
            let term_v  = Logic_const.tvar lv in
            let id_v = Logic_const.new_identified_term term_v  in
            let assigns_v = AAssigns ([], Writes ([(id_v,FromAny)])) in
            let ca_assigns_v = Logic_const.new_code_annotation assigns_v in
            let old_kf = Extlib.the self#current_kf in
            let kf =
              Visitor_behavior.Get.kernel_function self#behavior old_kf
            in
            let queued_action () =
               Annotations.add_code_annot ~keep_empty:true
                 wp_emitter ~kf new_stmt ca_assigns_v
            in
            Queue.add queued_action self#get_filling_actions
          end;
          ignore(Stack.pop inLoop);
          if Stack.is_empty inLoop then
            seen := false;
          new_stmt
        )
    | _ ->
      begin
        match is_stmt_by_sid stmt.sid with
        | l when l = [] || List.exists (fun ids' -> ids = ids') l ->
          Cil.DoChildrenPost (fun new_stmt ->
              match is_annotable_stmt_by_sid stmt.sid with
              | Some ids' when ids' = ids ->
                let len = (get_len ids) in
                let pos = get_pos ids stmt.sid in
                if pos = len then
                  self#add_sequence_annot ~loc:loc new_stmt (len-1)
                else if pos = 1 then
                  Commons.replace_or_add_list to_add stmt.sid
                    (self#mk_set ~loc pos)
                else
                  Commons.replace_or_add_list to_add stmt.sid
                    (self#mk_if ~loc pos);
                new_stmt
              | _ ->
                if is_cond ids stmt.sid then begin
                  Commons.replace_or_add_list to_add stmt.sid
                    (self#mk_set ~loc 0);
                  new_stmt
                end
                else
                  new_stmt
            )
        | _  ->
          to_remove <- stmt.sid :: to_remove;
          Cil.JustCopy
      end

  method private add_sequence_annot ?(loc=unk_loc) new_stmt value =
    let lvarInfo = Cil.cvar_to_lvar vInfo in
    let tvarInfo = Logic_const.tvar ~loc lvarInfo in
    let value = Logic_const.tinteger ~loc value in
    let pred = Logic_const.prel (Rneq,tvarInfo,value) in
    let code_annot = Logic_const.new_code_annotation (AAssert ([],Check,pred)) in
    (* /*@ check vInfo != value; */  *)
    let old_kf = Extlib.the self#current_kf in
    let new_kf = Visitor_behavior.Get.kernel_function self#behavior old_kf in
    let queued_action () =
      Annotations.add_code_annot ~kf:new_kf wp_emitter new_stmt code_annot;
      let ip = Property.ip_of_code_annot_single new_kf new_stmt code_annot in
      sinfo := Some ip;
    in
    Queue.add queued_action self#get_filling_actions

end

let create_project_for_sequence ?name ids =
  let name =
    match name with
    | None -> "sequence_"^string_of_int ids
    | Some name -> name
  in
  let sinfo = ref None in
  Hashtbl.reset to_add;
  let prj = File.create_project_from_visitor name
      (new sequence_selector ids sinfo)
  in
  prj, !sinfo
