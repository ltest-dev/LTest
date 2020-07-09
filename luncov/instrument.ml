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
open Commons

type id = int

(* type used to describe labels *)
type lbl_info = {
  li_loc : Cil_types.location;
  li_tag : string;
  li_annotable : Cil_types.stmt;
  li_predicate : Cil_types.exp;
  li_kf_id : int;
}

(* type used to describe sequence labels *)
type seq_info = {
  si_loc: Cil_types.location;
  si_pos : int;
  si_annotable : Cil_types.stmt;
}
(* type used to describe sequences *)
type seqs_info = {
  ssi_len : int;
  ssi_var : string;
  ssi_kf_id : int;
  mutable ssi_seqs : seq_info list;
  mutable ssi_conds : int list;
}

(* type used to describe binding labels *)
type bind_info = {
  bi_id : int;
  bi_loc : Cil_types.location;
  bi_tag : string;
  bi_annotable : Cil_types.stmt;
  bi_predicate : Cil_types.exp;
  bi_kf_id : int;
  bi_bindings : (string*Cil_types.exp) list;
}

type info = Label of lbl_info | Sequence of seqs_info | Binding of bind_info list

module Info = Datatype.Make (struct
    include Datatype.Serializable_undefined
    type t = info
    let name = "Instr.info"
    let reprs = [Label ({
        li_loc = Cil_datatype.Location.unknown;
        li_tag = "";
        li_annotable = Cil.dummyStmt;
        li_predicate = Cil_datatype.Exp.dummy;
        li_kf_id = -1;
      })]
    let mem_project =
      Datatype.never_any_project

  end)
module H = Datatype.Int.Hashtbl

module Infos =
  State_builder.Option_ref
    (Datatype.Triple (H.Make (Info))
       (H.Make (Datatype.Int))
       (H.Make (Datatype.Int)))
    (struct
      let name = "LabelInfos"
      let dependencies = [ Ast.self ]
    end)

let wp_emitter =
  Emitter.create ("Luncov_WP")
    [ Emitter.Code_annot ] ~correctness:[] ~tuning:[]
let eva_emitter =
  Emitter.create ("Luncov_EVA")
    [ Emitter.Code_annot ] ~correctness:[] ~tuning:[]

(* Visit the file, and for each label, sequence or binding, creates and adds it to
   the Instrument hashtbl *)
class mapper h blocks calls conds = object(self)
  inherit Visitor.frama_c_inplace

  val mutable opened_blocks = []
  val mutable previous_binding = []
  val mutable current_binding_id = -1

  method! vfunc _ =
    Cil.DoChildrenPost ( fun f ->
        self#save_binding (); (* Work around for the last binding seen *)
        f
      )

  method private save_binding () =
    if List.length previous_binding >= 2 then begin
      let first_id = (List.hd previous_binding).bi_id in
      H.add h first_id (Binding previous_binding);
      previous_binding <- [];
    end

  (* Used to get the block that contains the label (cf. labels.h) *)
  method private get_parent_and_current =
    match self#current_stmt, opened_blocks with
    | Some stmt, block :: _ -> Some (block.sid, stmt)
    | _ -> None

  method! vstmt_aux s =
    match s.skind with
    | Block _ ->
      opened_blocks <- s :: opened_blocks;
      Cil.ChangeDoChildrenPost (s, fun s -> opened_blocks <- List.tl opened_blocks; s)
    | _ ->
      Cil.DoChildren

  method private mk_label idexp cond tagexp loc =
    match Cil.isInteger idexp, cil_isString tagexp, self#get_parent_and_current  with
    | Some id, Some tag, Some (block_sid,call_stmt) ->
      let id = Integer.to_int id in
      let englobing_kf = Extlib.the self#current_kf in
      H.add blocks block_sid id;
      H.add calls call_stmt.sid id;
      Datatype.Int.Hashtbl.add h id (
        Label {
          li_loc=loc;
          li_tag=tag;
          li_annotable=call_stmt;
          li_predicate=cond;
          li_kf_id=Kernel_function.get_id englobing_kf;
        })
    | None,_,_ ->
      Options.warning "instr: invalid label at line %d [id]" (fst loc).Filepath.pos_lnum
    | _,None,_ ->
      Options.warning "instr: invalid label at line %d [tag]" (fst loc).Filepath.pos_lnum
    | _,_,None ->
      Options.warning "instr: invalid label at line %d [structure]" (fst loc).Filepath.pos_lnum

  method private mk_sequence  idsexp posexp lenexp varexp loc =
    match Cil.isInteger idsexp, Cil.isInteger posexp, Cil.isInteger lenexp, cil_isString varexp, self#get_parent_and_current with
    | Some ids, Some pos, Some len, Some var, Some (block_sid,call_stmt)  ->
      let englobing_kf = Extlib.the self#current_kf in
      let ids = Integer.to_int ids in
      let pos = Integer.to_int pos in
      let len = Integer.to_int len in
      let seq = {
        si_loc=loc;
        si_pos=pos;
        si_annotable=call_stmt;
      }
      in
      H.add blocks block_sid ids;
      H.add calls call_stmt.sid ids;
      if not (H.mem h ids) then
        H.add h ids (
          Sequence {
            ssi_len=len;
            ssi_var=var;
            ssi_kf_id=Kernel_function.get_id englobing_kf;
            ssi_seqs=[seq];
            ssi_conds=[];
          })
      else begin
        let info = H.find h ids in
        match info with
        | Sequence sinfo ->
          sinfo.ssi_seqs <- seq :: sinfo.ssi_seqs
        | _ -> assert false
      end
    | None,_,_,_,_ ->
      Options.warning "instr: invalid sequence at line %d [ids]" (fst loc).Filepath.pos_lnum
    | _,None,_,_,_ ->
      Options.warning "instr: invalid sequence at line %d [pos]" (fst loc).Filepath.pos_lnum
    | _,_,None,_,_ ->
      Options.warning "instr: invalid sequence at line %d [len]" (fst loc).Filepath.pos_lnum
    | _,_,_,None,_ ->
      Options.warning "instr: invalid sequence at line %d [var]" (fst loc).Filepath.pos_lnum
    | _,_,_,_,None ->
      Options.warning "instr: invalid sequence at line %d [structure]" (fst loc).Filepath.pos_lnum

  method private mk_sequence_cond var loc =
    match cil_isString var, self#get_parent_and_current with
    | Some var, Some (block_stmt,call_stmt)  ->
      if Hashtbl.mem conds var then begin
        let old = Hashtbl.find conds var in
        Hashtbl.replace conds var ((block_stmt,call_stmt.sid) :: old)
      end
      else Hashtbl.add conds var [(block_stmt, call_stmt.sid)]
    | None, _ ->
      Options.warning "instr: invalid sequence condition at line %d [var]" (fst loc).Filepath.pos_lnum
    | _,None ->
      Options.warning "instr: invalid sequence condition at line %d [structure]" (fst loc).Filepath.pos_lnum

  method private mk_binding_aux bind_list loc =
    let rec aux acc l =
      match l with
      | [] -> acc
      | [_] ->
        Options.warning "instr: invalid binding at line %d [(Name,Value)]" (fst loc).Filepath.pos_lnum;
        []
      | x :: y :: t ->
        begin
          match cil_isString x with
          | None ->
            Options.warning "instr: invalid binding at line %d [Name]" (fst loc).Filepath.pos_lnum;
            []
          | Some n -> aux ((n,y)::acc) t
        end
    in
    List.rev (aux [] bind_list)

  method private mk_binding idexp cond tagexp bindidexp nbBindsexp bind_list loc =
    match Cil.isInteger idexp, cil_isString tagexp, Cil.isInteger bindidexp, Cil.isInteger nbBindsexp ,self#get_parent_and_current  with
    | Some id, Some tag, Some bindId, Some _nbBinds, Some (block_sid,call_stmt) ->
      let id = Integer.to_int id in
      let bindId = Integer.to_int bindId in
      let englobing_kf = Extlib.the self#current_kf in
      let bind_list = self#mk_binding_aux bind_list loc in
      if bind_list != [] then begin
        H.add blocks block_sid id;
        H.add calls call_stmt.sid id;
        let bind =  {
          bi_id = id;
          bi_loc=loc;
          bi_tag=tag;
          bi_annotable=call_stmt;
          bi_kf_id=Kernel_function.get_id englobing_kf;
          bi_bindings=bind_list;
          bi_predicate=cond;
        } in
        if current_binding_id = -1 then current_binding_id <- bindId;
        if current_binding_id = bindId then
          previous_binding <- previous_binding @ [bind]
        else begin
          self#save_binding ();
          current_binding_id <- bindId;
          previous_binding <- [bind];
        end
      end

    | None,_,_,_,_ ->
      Options.warning "instr: invalid binding at line %d [id]" (fst loc).Filepath.pos_lnum
    | _,None,_,_,_ ->
      Options.warning "instr: invalid binding at line %d [tag]" (fst loc).Filepath.pos_lnum
    | _,_,None,_,_ ->
      Options.warning "instr: invalid binding at line %d [realId]" (fst loc).Filepath.pos_lnum
    | _,_,_,None,_ ->
      Options.warning "instr: invalid binding at line %d [nbBinds]" (fst loc).Filepath.pos_lnum
    | _,_,_,_,None ->
      Options.warning "instr: invalid binding at line %d [structure]" (fst loc).Filepath.pos_lnum

  method! vinst i =
    begin
      match i with
      | Call (None, {enode=(Lval (Var {vname=name}, NoOffset))}, idexp::cond::tagexp::_, loc)
        when name = label_function_name ->
        self#mk_label idexp cond tagexp loc
      | Call (_, {enode=Lval (Var {vname=name}, NoOffset)}, _::idsexp::posexp::lenexp::varexp::_, loc)
        when name = seq_function_name ->
        self#mk_sequence idsexp posexp lenexp varexp loc
      | Call (_, {enode=Lval (Var {vname=name}, NoOffset)}, _::var::[], loc)
        when name = seq_cond_function_name ->
        self#mk_sequence_cond var loc
      | Call (None, {enode=(Lval (Var {vname=name}, NoOffset))}, idexp::cond::tagexp::bind_id::nbBinds::bind_list, loc)
        when name = bind_function_name ->
        self#mk_binding idexp cond tagexp bind_id nbBinds bind_list loc
      | _ -> ()
    end;
    Cil.SkipChildren

end

let size_table = ref 0

(* Fill the hashtbl with the visitor, and then fill blocks and calls
   Hahstbl depending on label's type *)
let compute () : Infos.data =
  let ast = Ast.get () in
  let h = H.create 97 in
  let conds = Hashtbl.create 97 in
  let blocks = H.create 97 in
  let calls = H.create 97 in
  Visitor.visitFramacFileSameGlobals (new mapper h blocks calls conds) ast;
  let f id info =
    match info with
    | Sequence ssinfo ->
      (* Adds all sequence conditions statement to their corresponding
         sequences *)
      if Hashtbl.mem conds ssinfo.ssi_var then begin
        let conds = Hashtbl.find conds ssinfo.ssi_var in
        let cond_blocks, cond_calls = List.split conds in
        ssinfo.ssi_conds <- cond_calls;
        List.iter (fun bsid -> H.add blocks bsid id) cond_blocks
      end
    | _ -> ()
  in
  H.iter f h;
  size_table := H.length h;
  h, blocks, calls

let compute () =
  Infos.memo compute

let get id =
  let table,_,_ = compute () in
  H.find table id

let iter f =
  let table,_,_ = compute () in
  H.iter f table

let iter_lbls f =
  let table,_,_ = compute () in
  let f_aux key value =
    match value with
    | Label l ->
      f key l
    | _ -> ()
  in
  H.iter f_aux table

let iter_seqs f =
  let table,_,_ = compute () in
  let f_aux key value =
    match value with
    | Sequence s ->
      f key s
    | _ -> ()
  in
  H.iter f_aux table

let iter_binds f =
  let table,_,_ = compute () in
  let f_aux key value =
    match value with
    | Binding b ->
      f key b
    | _ -> ()
  in
  H.iter f_aux table

let is_annotable_stmt_by_sid sid =
  let _,_,calls = compute () in
  if H.mem calls sid then Some (H.find calls sid)
  else None

let is_stmt_by_sid sid =
  let _,blocks,_ = compute () in
  if H.mem blocks sid then H.find_all blocks sid
  else []
