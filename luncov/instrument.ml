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

open Cil_types
open Commons

type id = int

type info = {
  li_loc : Cil_types.location;
  li_tag : string;
  li_stmt : Cil_types.stmt;
  li_annotable : Cil_types.stmt;
  li_predicate : Cil_types.exp;
  li_kf : Cil_types.kernel_function;
}


module LabelInfo = Datatype.Make (struct
    include Datatype.Serializable_undefined
    type t = info
    let name = "Instr.info"
    let reprs = [ {
        li_loc = Cil_datatype.Location.unknown;
        li_tag = "";
        li_stmt = Cil.dummyStmt;
        li_annotable = Cil.dummyStmt;
        li_predicate = Cil_datatype.Exp.dummy;
        li_kf = Kernel_function.dummy ();
      } ]
    let mem_project =
      Datatype.never_any_project

  end)
module H = Datatype.Int.Hashtbl

module LabelInfos =
  State_builder.Option_ref
    (Datatype.Triple (H.Make (LabelInfo))
       (H.Make (Datatype.Int))
       (H.Make (Datatype.Int)))
    (struct
      let name = "LabelInfos"
      let dependencies = [ Ast.self ]
    end)


class label_mapper h = object(self)
  inherit Cil.nopCilVisitor

  val mutable current_kf = None
  val mutable opened_blocks = []
  method! vglob g =
    begin match g with
      | GFun (fd, _) ->
        (try
           let kf = Globals.Functions.get fd.svar in
           current_kf <- Some kf;
         with Not_found ->
           Kernel.fatal "No kernel function for function %a"
             Cil_datatype.Varinfo.pretty fd.svar)
      | _ ->
        ()
    end;
    Cil.DoChildren

  method! vstmt s =
    match s.skind with
    | Block _ ->
      opened_blocks <- s :: opened_blocks;
      Cil.ChangeDoChildrenPost (s, fun s -> opened_blocks <- List.tl opened_blocks; s)
    | _ ->
      Cil.DoChildren

  method private get_parent_and_current =
    match self#current_stmt, opened_blocks with
    | Some stmt, block ::_ -> Some (block, stmt)
    | _ -> None

  method! vinst i =
    begin
      match i with
      | Call (None, {enode=(Lval (Var {vname=name}, NoOffset))}, idexp::cond::tagexp::_, loc)
        when name = label_function_name ->
        begin
          match Cil.isInteger idexp, cil_isString tagexp, self#get_parent_and_current  with
          | Some id, Some tag, Some (block_stmt,call_stmt) ->
            let id = Integer.to_int id in
            let englobing_kf = Extlib.the current_kf in
            Datatype.Int.Hashtbl.add h id {
              li_loc=loc;
              li_tag=tag;
              li_stmt=block_stmt;
              li_annotable=call_stmt;
              li_predicate=cond;
              li_kf=englobing_kf;
            }
          | None,_,_ ->
            Options.warning "instr: invalid label at line %d [id]" (fst loc).Lexing.pos_lnum
          | _,None,_ ->
            Options.warning "instr: invalid label at line %d [tag]" (fst loc).Lexing.pos_lnum
          | _,_,None ->
            Options.warning "instr: invalid label at line %d [structure]" (fst loc).Lexing.pos_lnum
        end
      | _ -> ()
    end;
    Cil.SkipChildren
end

let compute () : LabelInfos.data =
  let ast = Ast.get () in
  let h = H.create 97 in
  Cil.visitCilFile (new label_mapper h) ast;
  let blocks = H.create 97 in
  let calls = H.create 97 in
  let f id info =
    H.add blocks info.li_stmt.sid id;
    H.add calls info.li_annotable.sid id;
  in
  H.iter f h;
  h, blocks, calls

let compute () =
  LabelInfos.memo compute

let get id =
  let table,_,_ = compute () in
  H.find table id

let iter f =
  let table,_,_ = compute () in
  H.iter f table

let get_loc id =
  (get id).li_loc

let get_tag id =
  (get id).li_tag

let get_stmt id =
  (get id).li_stmt

let get_annotable_stmt id =
  (get id).li_annotable

let get_predicate id =
  (get id).li_predicate

let get_kf id =
  (get id).li_kf

let is_annotable_stmt_by_sid sid =
  let _,_,calls = compute () in
  if H.mem calls sid then Some (H.find calls sid)
  else None

let is_annotable_stmt stmt =
  is_annotable_stmt_by_sid stmt.sid

let is_stmt_by_sid sid =
  let _,blocks,_ = compute () in
  if H.mem blocks sid then Some (H.find blocks sid)
  else None

let is_stmt stmt =
  is_stmt_by_sid stmt.sid

let at = ref Req

(** Emitter for uncoverable label's asertions *)
let emitter = Emitter.create "Luncov" [ Emitter.Code_annot ] ~correctness:[] ~tuning:[]

(** Visitor that removes all but a single label (given its id),
    add a single assertion whose validity implies the uncoverability of a label,
    and provides information to get a Property.t
*)
class label_selector lblid info prj = object (self)
  inherit Visitor.frama_c_copy prj

  val label_kf = get_kf lblid
  val mutable into_lbl_fun = false

  method private clean_formals slocals =
    let regexp = Str.regexp "__pc__label_cond_" in
    List.fold_left
      (fun nl v ->
         if Str.string_match regexp v.vname 0 then begin
           match last_element (String.split_on_char '_' v.vname) with
           | None -> v :: nl
           | Some s ->
             let id = int_of_string s in
             if id = lblid then v :: nl else nl
         end
         else v :: nl
      ) [] slocals

  method! vfunc f =
    let kf = Extlib.the self#current_kf in
    into_lbl_fun <- kf == label_kf;
    Cil.DoChildrenPost (fun f ->
        f.slocals <- self#clean_formals f.slocals;
        f
      )

  method! vstmt_aux stmt =
    match is_stmt_by_sid stmt.sid with
    | Some lblid' when lblid' <> lblid ->
      stmt.skind <- Instr (Skip (Cil_datatype.Stmt.loc stmt));
      Cil.SkipChildren
    | _ ->
      match is_annotable_stmt_by_sid stmt.sid with
      | Some lblid' when lblid' = lblid ->
        self#add_label_annot stmt;
        Cil.JustCopy
      | _ ->
        Cil.DoChildren

  method private add_label_annot new_stmt =
    let lblinfo = get lblid in
    let cond = Visitor.visitFramacExpr (self :> Visitor.frama_c_visitor) lblinfo.li_predicate in
    let cond = (Logic_utils.expr_to_term ~cast:false cond) in
    let assertion = Logic_const.prel (!at, cond, Cil.lzero ()) in (* NB negated*)
    let old_kf = Extlib.the self#current_kf in
    let new_kf = Cil.get_kernel_function self#behavior old_kf in
    let queued_action () =
      let code_annot = AAssert ([], assertion) in
      let code_annotation = Logic_const.new_code_annotation code_annot in
      Annotations.add_code_annot ~kf:new_kf emitter new_stmt code_annotation;
      info := Some (new_kf, new_stmt, code_annotation);
    in
    Queue.add queued_action self#get_filling_actions

end

let create_project_for_label ?name id =
  let name = match name with None -> "label_"^string_of_int id | Some name -> name in
  let info = ref None in
  let prj = File.create_project_from_visitor name (new label_selector id info) in
  match !info with
  | Some (kf,stmt,code_annotation) ->
    let ip = Property.ip_of_code_annot_single kf stmt code_annotation in
    prj, Some ip
  | None ->
    prj, None
