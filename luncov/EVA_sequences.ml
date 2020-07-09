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

(* Bind each sequence to the corresponding function *)
let seqPerFun : (int, int list) Hashtbl.t = Hashtbl.create 32
(* Bind each sequence to its var (var field in pc_label_sequence) *)
let seqPerVar : (string, int list) Hashtbl.t = Hashtbl.create 32
(* Bind each varinfo to its corresponding sequence *)
let seqvInfo : (int, varinfo) Hashtbl.t = Hashtbl.create 32

(* Each tuple represente a sequence ID and an assertion (check) which,
   if valid, means this sequence is uncoverable *)
let seq_ips : (int * Property.identified_property) list ref = ref []


(* Get the varinfo of a sequence given its id *)
let get_vinfo (ids:int) : Cil_types.varinfo =
  Hashtbl.find seqvInfo ids

(* Get all varinfo related to a var (var field in pc_label_sequence) via sequences ids *)
let get_vInfos_of_var (var:string) : Cil_types.varinfo list =
  let l = Hashtbl.find seqPerVar var in
  List.map (fun ids -> Hashtbl.find seqvInfo ids) l

(* Default unknown loc *)
let unk_loc = Cil_datatype.Location.unknown

(** Check all sequences, and add the valid ones to hashtbls seqPerVar and seqPerFun *)
class countSeq = object(self)
  inherit Visitor.frama_c_inplace

  val mutable current_f = -1
  val currentFunSeqs : (int,string * int * int ref) Hashtbl.t = Hashtbl.create 32

  method private addSeq (ids:int) (idv:string) (len:int) : unit =
    if Hashtbl.mem currentFunSeqs ids then
      let _,_,cpt = Hashtbl.find currentFunSeqs ids in
      incr cpt
    else
      Hashtbl.add currentFunSeqs ids (idv,len,ref 1)

  (* Check sequences for each function, and fill hashtbls *)
  method! vfunc (dec : Cil_types.fundec) : Cil_types.fundec Cil.visitAction =
    Hashtbl.clear currentFunSeqs;
    current_f <- dec.svar.vid;
    Cil.DoChildrenPost (fun f ->
        let iter ids (var,len,nbS) =
          if !nbS < len then
            Options.warning "Only %d member(s) found for the sequence %d of length %d, sequence will be ignored" !nbS ids len
          else if !nbS > len then
            Options.warning "%d members found for the sequence %d of length %d, sequence will be ignored" !nbS ids len
          else begin
            replace_or_add_list seqPerVar var ids;
            replace_or_add_list seqPerFun current_f ids
          end
        in
        Hashtbl.iter iter currentFunSeqs;
        f
      )

  method! vinst (instr : Cil_types.instr) : Cil_types.instr list Cil.visitAction =
    begin match instr with
      | Call (_, {enode=Lval (Var {vname=name}, NoOffset)}, _::ids::_::len::var::_, loc)
        when name = seq_function_name ->
        begin
          match Cil.isInteger ids, Cil.isInteger len, cil_isString var  with
          | Some ids, Some len, Some var ->
            let ids = Integer.to_int ids in
            let len = Integer.to_int len in
            self#addSeq ids var len
          | None,_,_ ->
            Options.warning "instr: invalid sequence at line %d [ids]" (fst loc).Filepath.pos_lnum
          | _,None,_ ->
            Options.warning "instr: invalid sequence at line %d [len]" (fst loc).Filepath.pos_lnum
          | _,_,None ->
            Options.warning "instr: invalid sequence at line %d [var]" (fst loc).Filepath.pos_lnum
        end
      | _ -> ()
    end;
    Cil.SkipChildren
end

(** This visitor insturment the code in order to check for each sequence if it is uncoverable or not
 * Steps :
 * 1. Create a new variable for each sequence at the beginning of each function (initialized to 0)
 * 2. If we see the first part of a sequence, then we set the corresponding variable to 1
 * 3. If we see the n part of a sequence, the we check if the variable is equal to (n-1), and if so, set it to n
 * 4. If we see the last part of a sequence, we create an annotation that check if the corresponding variable is equal to (n-1) with n the length of the sequence
 * 5. If we see a condition (pc_label_sequence_condition), then we set all variables corresponding so sequences affected by this condition to 0
 **)

class instrument prj = object(self)
  inherit Visitor.frama_c_copy prj

  val mutable seqList = []
  val mutable previous = None
  val to_add = Hashtbl.create 32
  val is_seq = ref false
  val prev_block = ref None

  (* vInfo == value *)
  method private mk_cond ?(loc=unk_loc) vInfo value =
    let lval = Cil.new_exp ~loc (Lval (Cil.var vInfo)) in
    let test = Cil.integer ~loc value in
    Cil.mkBinOp ~loc Cil_types.Eq lval test

  (* vInfo = value; *)
  method private mk_set ?(loc=unk_loc) vInfo value =
    let lval = Cil.var vInfo in
    let new_value = Cil.integer ~loc value in
    let set = Ast_info.mkassign lval new_value loc in
    Cil.mkStmtOneInstr ~ghost:true set

  (* if (vInfo == value - 1) vInfo = value; *)
  method private mk_if ?(loc=unk_loc) vInfo value =
    let cond = self#mk_cond ~loc vInfo (value-1) in
    let set = self#mk_set ~loc vInfo value in
    let thenb = Cil.mkBlock [set] in
    let elseb = Cil.mkBlock [] in
    Cil.mkStmt ~ghost:true (If(cond,thenb,elseb,loc))

  (* Step 1 *)
  method! vfunc (dec : Cil_types.fundec) : Cil_types.fundec Cil.visitAction =
    if Hashtbl.mem seqPerFun dec.svar.vid then begin
      let l = Hashtbl.find seqPerFun dec.svar.vid in
      seqList <- List.sort compare l;
      Hashtbl.remove seqPerFun dec.svar.vid;
      let init =
        List.map ( fun ids ->
            let vi = Cil.makeVarinfo false true
                ("__SEQ_STATUS_" ^ string_of_int ids)
                (TInt(IInt,[]))
            in
            vi.vghost <- true;
            Hashtbl.add seqvInfo ids vi;
            vi
          ) seqList
      in
      Cil.DoChildrenPost (fun f ->
          let set = List.map (fun vi ->
              f.slocals <- f.slocals @ [vi];
              f.sbody.blocals <- f.sbody.blocals @ [vi];
              self#mk_set vi 0
            ) init
          in
          f.sbody.bstmts <- set@f.sbody.bstmts;
          f
        )
    end
    else
      Cil.JustCopy

  (* Add all new statement to their corresping location (i.e. before a statement) *)
  method! vblock _ = Cil.DoChildrenPost (fun block ->
      let rec aux = function
        | [] -> []
        | s :: t ->
          (List.rev (Hashtbl.find_all to_add s.sid)) @ [s] @ (aux t)
      in block.bstmts <- aux block.bstmts;
      block
    )

  method! vstmt_aux (stmt : Cil_types.stmt) : Cil_types.stmt Cil.visitAction =
    match stmt.skind with
    | Block _ ->
      (* Each sequence member is encapsulated inside a block. After visiting these blocks, we replaced them by the Skip instruction in order to improve EVA performances, as those blocks are not needed for the analyse itself once we have added our ghost codes and checks *)
      is_seq:=false;
      prev_block:=Some stmt;
      Cil.DoChildrenPost (fun b ->
          if !is_seq then begin
            b.skind <- Instr (Skip unk_loc);
            is_seq:=false;
          end;
          prev_block:=None;
          b
        )

    | Instr (Call (_, {enode=Lval (Var {vname=name}, NoOffset)}, _::ids::pos::len::_::_, loc))
      when name = seq_function_name ->
      is_seq := true;
      begin
        match Cil.isInteger ids, Cil.isInteger pos, Cil.isInteger len with
        | Some ids, Some pos, Some len ->
          let ids = Integer.to_int ids in
          if List.exists (fun ids' -> ids' = ids) seqList then begin
            let pos = Integer.to_int pos in
            let len = Integer.to_int len in
            let vInfo = get_vinfo ids in
            if pos = len then (* Step 4 *)
              self#add_sequence_annot ~loc:loc ids vInfo (len-1)
            else if pos = 1 then (* Step 2 *)
              Hashtbl.add to_add (Extlib.the !prev_block).sid (self#mk_set ~loc vInfo pos)
            else (* Step 3 *)
              Hashtbl.add to_add (Extlib.the !prev_block).sid (self#mk_if ~loc vInfo pos)
          end
        | _ ->
          Options.warning "instr: invalid sequence at line %d " (fst loc).Filepath.pos_lnum
      end;
      Cil.JustCopy
    | Instr (Call (_, {enode=Lval (Var {vname=name}, NoOffset)}, _::var::[], loc))
      when name = seq_cond_function_name ->
      is_seq := true;
      begin
        match cil_isString var with
        | Some var ->
          (* Step 5 *)
          let l = get_vInfos_of_var var in
          List.iter (fun vInfo ->
              Hashtbl.add to_add (Extlib.the !prev_block).sid (self#mk_set ~loc vInfo 0)
            ) l;
        | _ ->
          Options.warning "instr: invalid sequence condition at line %d" (fst loc).Filepath.pos_lnum
      end;
      Cil.JustCopy
    | _ ->
      Cil.DoChildren

  method private add_sequence_annot ?(loc=unk_loc) ids vInfo value =
    let stmt = Extlib.the !prev_block in
    let lvarInfo = Cil.cvar_to_lvar vInfo in
    let tvarInfo = Logic_const.tvar ~loc lvarInfo in
    let value = Logic_const.tinteger ~loc value in
    let pred = Logic_const.prel (Rneq,tvarInfo,value) in
    (* /*@ vInfo != value; */  *)
    let check = Logic_const.new_code_annotation (AAssert ([],Check,pred)) in
    let old_kf = Extlib.the self#current_kf in
    let new_kf = Visitor_behavior.Get.kernel_function self#behavior old_kf in
    let queued_action () =
      Annotations.add_code_annot Instrument.eva_emitter ~kf:new_kf stmt check;
      let ips = Property.ip_of_code_annot_single new_kf stmt check in
      seq_ips := (ids,ips) :: !seq_ips
    in
    Queue.add queued_action self#get_filling_actions
end

(* Create a new project with the code instrumented if there's at least 1 sequence or binding in the code *)
let instrument_sequences () =
  Visitor.visitFramacFileSameGlobals (new countSeq) (Ast.get());
  if Hashtbl.length seqPerFun <> 0  then begin
    let new_proj = File.create_project_from_visitor "EVA_sequences" (new instrument) in
    Project.copy ~selection:(Parameter_state.get_selection()) new_proj;
    Project.set_current new_proj
  end
