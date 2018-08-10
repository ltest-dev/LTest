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

open Cil
open Cil_types
open Ast_const

let aorOption : bool ref = ref false
let rorOption : bool ref = ref false
let corOption : bool ref = ref false
let absOption : bool ref = ref false

class mutationVisitor mk_label = object(self)
  inherit Visitor.frama_c_inplace

  val mutable exprs = []
  val mutable currloc = Cil_datatype.Location.unknown

  method private makeLabel cond category =
    let le = mk_label ~extra:[category] cond [] currloc in
    exprs <- exprs @ [le]

  method private mk_op_labels e lop lexp rexp ty wm opt =
    if opt = true then
      List.iter (fun op ->
          let newExp = Ast_const.Exp.mk(BinOp(op, lexp, rexp, ty)) in
          let labelExp = Ast_const.Exp.mk(BinOp(Ne, newExp, e, ty)) in
          self#makeLabel labelExp wm
        ) lop;
    self#traitExp lexp;
    self#traitExp rexp


  method private traitExp e =
    match e.enode with
    | BinOp(LAnd, lexp, rexp, ty) ->
      self#mk_op_labels e [LOr] lexp rexp ty "COR" !corOption
    | BinOp(LOr, lexp, rexp, ty) ->
      self#mk_op_labels e [LAnd] lexp rexp ty "COR" !corOption
    | BinOp(Div, lexp, rexp, ty) ->
      self#mk_op_labels e [Mult;PlusA;MinusA] lexp rexp ty "AOR" !aorOption
    | BinOp(Mult, lexp, rexp, ty) ->
      self#mk_op_labels e [Div;PlusA;MinusA] lexp rexp ty "AOR" !aorOption
    | BinOp(PlusA, lexp, rexp, ty) ->
      self#mk_op_labels e [Mult;Div;MinusA] lexp rexp ty "AOR" !aorOption
    | BinOp(MinusA, lexp, rexp, ty) ->
      self#mk_op_labels e [Mult;Div;PlusA] lexp rexp ty "AOR" !aorOption
    | BinOp(Lt, lexp, rexp, ty) ->
      self#mk_op_labels e [Le;Gt;Ge] lexp rexp ty "ROR" !rorOption
    | BinOp(Gt, lexp, rexp, ty) ->
      self#mk_op_labels e [Lt;Le;Ge] lexp rexp ty "ROR" !rorOption
    | BinOp(Le, lexp, rexp, ty) ->
      self#mk_op_labels e [Lt;Gt;Ge] lexp rexp ty "ROR" !rorOption
    | BinOp(Ge, lexp, rexp, ty) ->
      self#mk_op_labels e [Lt;Le;Gt] lexp rexp ty "ROR" !rorOption
    | BinOp(Eq, lexp, rexp, ty) ->
      self#mk_op_labels e [Ne] lexp rexp ty "ROR" !rorOption
    | BinOp(Ne, lexp, rexp, ty) ->
      self#mk_op_labels e [Eq] lexp rexp ty "ROR" !rorOption
    | BinOp(Shiftlt, lexp, rexp, ty) ->
      self#mk_op_labels e [Shiftrt] lexp rexp ty "AOR" !aorOption
    | BinOp(Shiftrt, lexp, rexp, ty) ->
      self#mk_op_labels e [Shiftlt] lexp rexp ty "AOR" !aorOption
    | BinOp(_op, lexp, rexp, _ty) ->
      self#traitExp lexp;
      self#traitExp rexp
    | UnOp(Neg, exp, ty) ->
      if !aorOption = true then begin
        let labelExp = Ast_const.Exp.mk(BinOp(Ne, exp, e, ty)) in
        self#makeLabel labelExp "AOR"
      end;
      self#traitExp exp
    | UnOp(_op, exp, _ty)->
      self#traitExp exp
    | Lval(_l) ->
      if !absOption = true then begin
        let zeroExp = Ast_const.Exp.mk (Const(CInt64(Integer.of_int(0),IInt,None))) in
        let labelExp = Ast_const.Exp.mk(BinOp(Lt, e, zeroExp, intType)) in
        self#makeLabel labelExp "ABS"
      end
    | _ -> ()

  method! vfunc f =
    if Annotators.shouldInstrument f.svar then DoChildren else SkipChildren

  method! vstmt_aux stmt =
    begin match stmt.skind with
      | Instr(Set(_l, e, loc)) ->
        currloc <- loc;
        self#traitExp e
      | If(e, _l, _ll, loc) ->
        currloc <- loc;
        self#traitExp e
      | Return(e, loc) ->
        begin match e with
          | Some exp ->
            currloc <- loc;
            self#traitExp exp
          | _ -> ()
        end
      | Switch(e, _l,_ll, loc) ->
        currloc <- loc;
        self#traitExp e
      | _ -> ()
    end;
    let stmtExprsLabels = exprs in
    exprs <- [];
    DoChildrenPost (fun stmt ->
        match stmtExprsLabels with
        | [] -> stmt
        | _ ->
          let finalList = List.append stmtExprsLabels [stmt] in
          Stmt.block finalList
      )
end

module WM = Annotators.RegisterWithExtraTags (struct
    let name = "WM"
    let help = "Weak Mutation"

    let apply mk_label ast =
      Visitor.visitFramacFileSameGlobals
        (new mutationVisitor mk_label :> Visitor.frama_c_inplace)
        ast
  end)
