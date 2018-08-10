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
open Ast_const

let idsListSLO = ref []
(*let idsListASL = ref []*)

(** Return True if the stmt list only contain 1 statement which is a Break *)
let is_break_only bstmts =
  List.length bstmts = 1 &&
  match (List.hd bstmts).skind with
  | Break _ -> true
  | _ -> false

(*
(** Add a label in each loop condition branches*)
let realloop = object(self)
  inherit Visitor.frama_c_inplace

  (** Pile qui stock le status des boucles *)
  val inLoopId = Stack.create ()

  method private mk_call () =
    let id = Annotators.next () in
    let idExp = Exp.integer id in
    let oneExp = Exp.one () in
    let ccExp = Exp.string "ASL" in
    let newStmt = (Utils.mk_call "pc_label" ([oneExp;idExp;ccExp])) in
    newStmt

  method! vfunc dec =
    if Annotators.shouldInstrument dec.svar then
      Cil.DoChildren
    else
      Cil.SkipChildren

  method! vstmt_aux stmt =
    match stmt.skind with
    | Loop (_,_,l,_,_) ->
      let labelThen = self#mk_call () in
      let id1 = Annotators.getCurrentLabelId() in
      let labelElse = self#mk_call () in
      let id2 = Annotators.getCurrentLabelId() in
      idsListASL := (id1,id2) :: !idsListASL;
      Cil.DoChildrenPost (fun stmt ->
          begin
            match stmt.skind with
            | Loop (ca,b,l,s1,s2) ->
              let found = ref false in
              (* Cf. doc/loops.markdown for more info *)
              let len = List.length b.bstmts in
              let f id s =
                match s.skind with
                | If (e,th,el,l) ->
                  (* If not found already && then is empty && else contains only break &&
                     (isn't the last stmt in the body, or we don't want to handle do..while..) *)
                  if not !found && th.bstmts = [] && is_break_only el.bstmts && (id+1 != len || not (Options.HandleDoWhile.get())) then begin
                    s.skind <- (If (e,{th with bstmts = [labelThen]},{el with bstmts = [labelElse]@(el.bstmts)},l));
                    found := true
                  end;
                  s
                | _ -> s
              in
              let nb = List.mapi f b.bstmts in
              if !found then
                b.bstmts <- nb
              else
                b.bstmts <- labelThen :: labelElse :: b.bstmts;
              stmt
            | _ -> assert false
          end
        )
    | _ ->
      Cil.DoChildren
end
*)

(** Add a label in each loop, to see if we enter in it *)
let inner mk_label = object(_)
  inherit Visitor.frama_c_inplace

  (** Pile qui stock le status des boucles *)
  val inLoopId = Stack.create ()

  method! vfunc dec =
    if Annotators.shouldInstrument dec.svar then
      Cil.DoChildren
    else
      Cil.SkipChildren

  method! vstmt_aux stmt =
    match stmt.skind with
    | Loop (_,_,l,_,_) ->
      let label = mk_label (Exp.one()) [] l in
      Cil.DoChildrenPost (fun stmt ->
          begin
            match stmt.skind with
            | Loop (ca,b,l,s1,s2) ->
              let found = ref false in
              (* Cf. doc/loops.markdown for more info *)
              let len = List.length b.bstmts in
              let f id s =
                match s.skind with
                | If (e,th,el,l) ->
                  (* If not found already && then is empty && else contains only break &&
                     (isn't the last stmt in the body, or we don't want to handle do..while..) *)
                  if not !found && th.bstmts = [] && is_break_only el.bstmts && (id+1 != len || not (Options.HandleDoWhile.get())) then begin
                    s.skind <- (If (e,{th with bstmts = [label]},el,l));
                    found := true
                  end;
                  s
                | _ -> s
              in
              let nb = List.mapi f b.bstmts in
              if !found then
                b.bstmts <- nb
              else
                b.bstmts <- label :: b.bstmts;
              stmt
            | _ -> assert false
          end
        )
    | _ ->
      Cil.DoChildren
end

(** Create a pc_label_sequence *)
let mkSeq (sid : int) (lid : int) (nb : int) : Cil_types.stmt =
  let idExp = Exp.kinteger IULong sid in
  let oneExp = Exp.one () in
  let curr = Exp.integer nb in
  let slen = Exp.integer 2 in
  let varExp = Exp.string (string_of_int lid) in
  let zeroExp = Exp.zero () in
  Utils.mk_call "pc_label_sequence" ([oneExp;idExp;curr;slen;varExp;zeroExp])

(** Create a pc_label_sequence_condiion *)
let mkCond (sid : int) : Cil_types.stmt =
  let zeroExp = Exp.zero () in
  let ccExp = Exp.string (string_of_int sid) in
  Utils.mk_call "pc_label_sequence_condition" ([zeroExp;ccExp])

(** Add a label in each loop, to see if we enter in it *)
let outter () = object(_)
  inherit Visitor.frama_c_inplace

  (** Pile qui stock le status des boucles *)
  val inLoopId = Stack.create ()

  method! vfunc dec =
    if Annotators.shouldInstrument dec.svar then
      Cil.DoChildren
    else
      Cil.SkipChildren

  method! vstmt_aux stmt =
    match stmt.skind with
    | Loop (_,_,l,_,_) ->
      let sid = Annotators.next () in
      let set = mkSeq sid stmt.sid 1 in
      let use = mkSeq sid stmt.sid 2 in
      let cond = mkCond stmt.sid in
      idsListSLO := sid :: !idsListSLO;
      Cil.DoChildrenPost (fun stmt ->
          let newLoop =
            match stmt.skind with
            | Loop (ca,b,l,s1,s2) ->
              let found = ref false in
              (* Cf. doc/loops.markdown for more info *)
              let len = List.length b.bstmts in
              let f id s =
                match s.skind with
                | If (e,th,el,l) ->
                  (* If not found already && then is empty && else contains only break &&
                     (isn't the last stmt in the body, or we don't want to handle do..while..) *)
                  if not !found && th.bstmts = [] && is_break_only el.bstmts
                     && (id+1 != len || not (Options.HandleDoWhile.get ())) then begin
                    s.skind <- (If (e,{th with bstmts = [cond]},el,l));
                    found := true
                  end;
                  s
                | _ -> s
              in
              let nb = List.mapi f b.bstmts in
              if !found then
                b.bstmts <- nb
              else
                b.bstmts <- cond :: b.bstmts;
              stmt
            | _ -> assert false
          in
          Stmt.block [set;newLoop;use]
        )
    | _ ->
      Cil.DoChildren
end

(*
(** Use idList to create all hyprlabels *)
let compute_hl_ASL () : string =
    List.fold_left (fun str (id1,id2) -> "<s" ^ string_of_int id2 ^".!s"^string_of_int id1^"|; ;>,\n" ^ str) "" !idsListASL

let gen_hyperlabels_ASL () =
  let data_filename = (Filename.chop_extension (Annotators.get_file_name ())) ^ ".hyperlabels" in
  Options.feedback "write hyperlabel data (to %s)" data_filename;
  let data = compute_hl_ASL () in
  let out = open_out_gen [Open_creat; Open_append] 0o640 data_filename in
  output_string out data;
  close_out out;
  Options.feedback "Total number of labels = %d" ((List.length !idsListASL)*2)*)

(** Use idList to create all hyprlabels *)
let compute_hl_SLO () : string =
    List.fold_left (fun str ids -> "<s" ^ string_of_int ids ^"|; ;>,\n" ^ str) "" !idsListSLO

let gen_hyperlabels_SLO () =
  let data_filename = (Filename.chop_extension (Annotators.get_file_name ())) ^ ".hyperlabels" in
  Options.feedback "write hyperlabel data (to %s)" data_filename;
  let data = compute_hl_SLO () in
  let out = open_out_gen [Open_creat; Open_append] 0o640 data_filename in
  output_string out data;
  close_out out;
  Options.feedback "Total number of labels = %d" ((List.length !idsListSLO)*3)


let warning () =
  if Options.HandleDoWhile.get () then
    Options.warning "Handle Do While enabled : Empty loops will be considered as Do..While"
  else
    Options.warning "Handle Do While disabled : Do..While will be considered as Empty loops";

(*include Annotators.Register (struct

    let name = "ASL"
    let help = "Always Skip Loop Coverage"

    let apply _ ast =
      warning();
      Visitor.visitFramacFileSameGlobals realloop ast;
      gen_hyperlabels_ASL ()
  end)*)

include Annotators.Register (struct

    let name = "ELO"
    let help = "Enter in Loop at least once Coverage"

    let apply mk_label ast =
      warning();
      Visitor.visitFramacFileSameGlobals (inner mk_label) ast
  end)

include Annotators.Register (struct

    let name = "SLO"
    let help = "Skip Loop at least Once Coverage"

    let apply mk_label ast =
      warning();
      Visitor.visitFramacFileSameGlobals (outter ()) ast;
      gen_hyperlabels_SLO ()
  end)
