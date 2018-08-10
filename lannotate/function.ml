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

include Annotators.Register (struct

    let name = "FC"
    let help = "Function Coverage"

    (** A visitor that adds a label at the start of each function's body *)
    let visitor mk_label = object
      inherit Visitor.frama_c_inplace

      method! vfunc dec =
        if  Annotators.shouldInstrument dec.svar then begin
          let label = mk_label (Exp.one()) [] dec.svar.vdecl in
          dec.sbody.bstmts <- label :: dec.sbody.bstmts;
        end;
        Cil.SkipChildren
    end

    let apply f ast =
      Visitor.visitFramacFileSameGlobals (visitor f) ast

  end)


module StringString = struct
  type t = string * string
  let compare = compare
end
module HL = Set.Make(StringString)
let hyperlabels = ref HL.empty

let label_id = ref 0
let disjunctions = Hashtbl.create 100

let compute_hl caller_callee =
  let disj =
    String.concat "+" (List.rev (List.map (fun i -> "l" ^ string_of_int i) (Hashtbl.find_all disjunctions caller_callee)))
  in
  "<" ^ disj ^ "|; ;>,"

let gen_hyperlabels_callcov = ref (fun () ->
  let data_filename = (Filename.chop_extension (Annotators.get_file_name ())) ^ ".hyperlabels" in
  Options.feedback "write hyperlabel data (to %s)" data_filename;
  let out = open_out_gen [Open_creat; Open_append] 0o640 data_filename in
  output_string out (HL.fold (fun el str -> str ^ (compute_hl el) ^ "\n") !hyperlabels "");
  close_out out)


let mk_call v func =
  let id = Annotators.next () in
  Hashtbl.add disjunctions (func,v.vname) id;
  hyperlabels := (HL.add (func,v.vname) !hyperlabels);
  let idExp = Exp.integer id in
  let oneExp = Exp.one () in
  let ccExp = Exp.string "FCC" in
  let newStmt = (Utils.mk_call "pc_label" ([oneExp;idExp;ccExp])) in
  newStmt

(** Call Coverage Visitor **)
class visitor = object(_)
  inherit Visitor.frama_c_inplace

  val mutable current_func = ""

  method! vfunc dec =
    current_func <- dec.svar.vname;
    Cil.DoChildren

  method! vstmt_aux stmt =
    begin match stmt.skind with
      | Instr i when not (Utils.is_label i) ->
        begin match i with
          | Call (_,{eid = _;enode = Lval(Var v,_);eloc = _},_,_)
          | Local_init (_,ConsInit(v, _,_),_) ->
            let newStmt = mk_call v current_func in
            Cil.ChangeTo (Stmt.block [newStmt; stmt])
          | _ -> Cil.DoChildren
        end
      | _ -> Cil.DoChildren
    end

end

(**
   Call coverage annotator
*)
module CallCov = Annotators.Register (struct
    let name = "FCC"
    let help = "Function Call Coverage"
    let apply mk_label file =
      ignore mk_label;
      Visitor.visitFramacFileSameGlobals (new visitor :> Visitor.frama_c_visitor) file;
      !gen_hyperlabels_callcov ()
  end)

let cplTyToNbr = Hashtbl.create 100
let id_gen = ref 0
let list_convert = ref []
let oc = ref None
let get_oc () = match !oc with | Some f -> f | None -> let f = (open_out "output.txt") in oc := Some f; f



(** Remove Cast Visitor **)
class remcastvisitor = object(selfobj)
  inherit Visitor.frama_c_inplace

  val mutable current_func = Cil.emptyFunction ""
  val mutable newfuncs = ref []
  val mutable in_func = ref false

  method! vfile _ =
    let f file = (
    file.globals <- file.globals @ !newfuncs;
    file
    ) in (Cil.DoChildrenPost f)

  method! vfunc dec =
    current_func <- dec;
    in_func := true;
    let f res = in_func := false; res in
    Cil.DoChildrenPost f


  method! vexpr e = if !in_func then begin
      match e.enode with
      | CastE (_,_) ->
        let f res =
          match res.enode with
          | CastE (tt1,ce) ->
            let tt2 = Cil.typeOf ce in
            (Printer.pp_typ (Format.str_formatter) tt2) ;
            let t2 = (Format.flush_str_formatter ()) in
            (Printer.pp_typ (Format.str_formatter) tt1) ;
            let t1 = (Format.flush_str_formatter ()) in
            let nb_fun_repl =
              if Hashtbl.mem cplTyToNbr (t2,t1) then
                (Hashtbl.find cplTyToNbr (t2,t1))
              else begin
                incr id_gen;
                let nb = !id_gen in
                Hashtbl.add cplTyToNbr (t2,t1) !id_gen;
                Printf.fprintf (get_oc ()) "%s\n" ("/*@assigns \\nothing;*/ "^t1^" convert_"^(string_of_int nb)^"("^t2^" input);");
                let func = Cil.emptyFunction ("convert_"^(string_of_int nb)) in
                Cil.setFunctionTypeMakeFormals func (Cil_types.TFun(tt1, Some [("input",tt2,[])] , false, []));
                Cil.setFormals func [(Cil.makeFormalsVarDecl ("input",tt2,[]))];
                newfuncs := (GFun (func,func.svar.vdecl)):: !newfuncs;
                nb
              end
            in
            let convert_fun_name = "convert_"^(string_of_int nb_fun_repl) in
            let convert_result_info = (Cil.makeTempVar current_func tt1) in
            let convert_result = (Cil.var convert_result_info) in
            let convert_call = (Utils.mk_call ~result:convert_result convert_fun_name ([ce])) in
            list_convert := convert_call::!list_convert;
            (Cil.evar convert_result_info)
          | _ -> failwith "Unexpected"
        in (Cil.DoChildrenPost f)
      | _ -> Cil.DoChildren
    end
    else
      Cil.SkipChildren

  method! vstmt stmt =
    if !in_func then begin
      let saved_labels = stmt.labels in
      stmt.labels <- [];
      let follow () =
        begin
          let f res =
            stmt.labels <- saved_labels;
            if not ((List.length !list_convert)=0) then begin
              let listr = List.rev (!list_convert) in
              list_convert := [];
              (Stmt.block (listr @ [ res ]) )
            end
            else
              res
          in
          match stmt.skind with
          | Instr i ->
            (match i with
             | Call (Some lv,func , x,y) ->
               if (Cil.need_cast (Cil.typeOfLval lv) (Cil.getReturnType (Cil.typeOf func))) then begin
                 let temp_var = (Cil.makeTempVar current_func (Cil.getReturnType (Cil.typeOf func))) in
                 let new_call = (Cil.mkStmtOneInstr (Call (Some (Cil.var temp_var),func , x,y))) in
                 stmt.skind <- new_call.skind;
                 let new_assig = (Cil.mkStmtOneInstr (Set (lv , (Cil.mkCast (Cil.new_exp Cil_datatype.Location.unknown (Lval (Cil.var temp_var))) (Cil.typeOfLval lv)), Cil_datatype.Location.unknown))) in
                 let block = Stmt.block [stmt ; new_assig] in
                 let new_blok = Visitor.visitFramacStmt (selfobj :> Visitor.frama_c_visitor) block in
                 (Cil.ChangeTo new_blok)
               end
              else (Cil.DoChildrenPost f)
            | _ -> (Cil.DoChildrenPost f))
         | _ -> (Cil.DoChildrenPost f)
        end
      in
      match stmt.skind with
      | Instr i ->
        (match i with
         | Call (None,func , _,_) ->
           (match func.enode with
            | Lval (Var v,_) ->
              if v.vname = "__builtin_va_arg" then
                (stmt.labels <- saved_labels; Cil.SkipChildren)
              else
                follow ()
            | _ -> follow ())
         | _ -> follow ())
      | If (e, thenb, elseb, loc) ->
        (* handle visits manually to skip visit of e *)
        let thenb = Visitor.visitFramacBlock (selfobj :> Visitor.frama_c_visitor) thenb in
        let elseb = Visitor.visitFramacBlock (selfobj :> Visitor.frama_c_visitor) elseb in
        let e = Visitor.visitFramacExpr (selfobj :> Visitor.frama_c_visitor) e in
        stmt.labels <- saved_labels;
        stmt.skind <- If (e, thenb, elseb, loc);
        if not ((List.length !list_convert)=0) then begin
          let listr = List.rev (!list_convert) in
          list_convert := [];
          Cil.ChangeTo (Stmt.block (listr @ [ stmt ]) )
        end
        else
          Cil.ChangeTo stmt
      | Switch (e,block,ll,loc) ->
        let block = Visitor.visitFramacBlock (selfobj :> Visitor.frama_c_visitor) block in
        let e = Visitor.visitFramacExpr (selfobj :> Visitor.frama_c_visitor) e in
        stmt.labels <- saved_labels;
        stmt.skind <- Switch (e,block,ll,loc);
        if not ((List.length !list_convert)=0) then begin
          let listr = List.rev (!list_convert) in
          list_convert := [];
          Cil.ChangeTo (Stmt.block (listr @ [ stmt ]) )
        end
        else
          Cil.ChangeTo stmt
      | _ -> (follow ())
    end
    else
      Cil.SkipChildren
end


(**
   Remove cast annotator
*)
module CastRem = Annotators.Register (struct
    let name = "CastRem"
    let help = "Process file and replace casts by external function calls (useful for WP reasoning)"
    let apply _ file =
      Visitor.visitFramacFileSameGlobals (new remcastvisitor :> Visitor.frama_c_visitor) file
  end)



(** Nop Visitor **)
class nopvisitor = object(_)
  inherit Visitor.frama_c_inplace
end


(**
   Call coverage annotator
*)
module Empty = Annotators.Register (struct
    let name = "Empty"
    let help = "Process file but add no label"
    let apply _ file =
      Visitor.visitFramacFileSameGlobals (new nopvisitor :> Visitor.frama_c_visitor) file
  end)
