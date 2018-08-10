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

(** Number of defs for each variable *)
let nBVarDefs : (int, int) Hashtbl.t = Hashtbl.create 32
(** Current def for each variable *)
let currentDef : (int, int) Hashtbl.t  = Hashtbl.create 32

(** For each expression using many LVals, store all combination for these variables *)
let multiUses : (int, int list) Hashtbl.t  = Hashtbl.create 32
(** For each combination, store its length and the current position in the program *)
let combsID : (int, int*(int ref)) Hashtbl.t  = Hashtbl.create 32
(** For each couple (variable id, def id), store all combination in which it appears *)
let invertedPathID : (int*int, int list) Hashtbl.t  = Hashtbl.create 32
(** Store for each couple (variable id, loop id) a unique id *)
let varLoopID : (int*int, int) Hashtbl.t = Hashtbl.create 32

(** Store use labels *)
let labelUses : (stmt list) ref = ref []
(** Store def labels *)
let labelDefs : (stmt list) ref = ref []
(** Store conditions labels *)
let labelStops : (stmt list) ref = ref []
(** Store a tuplet (expr id, combination Id).
    Will be used to create hyperlabels *)
let idList : ((int*int) list) ref = ref []

(** Count the number of labels *)
let totalLabels = ref 0
(** Count the number of ignored labels *)
let ignoredLabels = ref 0

(** Hyperlabel's type *)
let symb = ref ""

(** Associates a unique id to a couple (Variable id, loop id) and store/return it (or return it if already exists *)
let get_varLoop_id (vid:int) (lid:int) : int =
  if Hashtbl.mem varLoopID (vid,lid) then
    Hashtbl.find varLoopID (vid,lid)
  else begin
    let new_id = Cil_const.new_raw_id () in
    Hashtbl.add varLoopID (vid,lid) new_id;
    new_id
  end

(** Return the cartesian product between all lists given in parameters*)
let rec n_cartesian_product (ll : (int*int) list list) =
  match ll with
  | [] -> assert false
  | [l] -> List.fold_left (fun acc i -> [i]::acc) [] l
  | h :: t ->
    let rest = n_cartesian_product t in
    List.concat
      (List.fold_left (fun acc i -> (List.fold_left (fun acc2 r -> (i :: r)::acc2) [] rest)::acc) [] h)

(** Count the number of LVals in an expression *)
class multiUseExp = object(_)
  inherit Visitor.frama_c_inplace

  val mutable id_LVals = []

  method get_LVals () = id_LVals

  method! vexpr expr =
    match expr.enode with
    | Lval (Var v,_) ->
      (** If the Lval is previously defined, and not already in the list *)
      if not v.vglob && not (v.vname = "__retres" ) && not v.vtemp && Hashtbl.mem nBVarDefs v.vid then
        if not (List.exists (fun id -> id = v.vid) id_LVals) then begin
          id_LVals <- v.vid :: id_LVals
        end;
      Cil.DoChildren
    | _ -> Cil.DoChildren
end

(** Visitor That count all Definitions for each variable *)
class countDef = object(self)
  inherit Visitor.frama_c_inplace

  (** Stack that store loops id wen entering in it, and pop it when leaving *)
  val inLoopId = Stack.create ()

  (** Fill nbVarDefs and currentDef with vid *)
  method private fill_aux (vid : int) : unit =
    if (Hashtbl.mem nBVarDefs vid) then
      (Hashtbl.replace nBVarDefs vid ((Hashtbl.find nBVarDefs vid) + 1))
    else
      (Hashtbl.add nBVarDefs vid 1; Hashtbl.add currentDef vid 1)

  (** Call fill_aux for the "normal" vid, and call it again if we're in a loop
      with the id assiociated to this variable id and loop id *)
  method private fill_def (vid : int) : unit =
    self#fill_aux vid;
    if not (Stack.is_empty inLoopId) then begin
      let lid = Stack.top inLoopId in
      let nvid = get_varLoop_id vid lid in
      self#fill_aux nvid
    end

  method! vfunc dec =
    if not (Annotators.shouldInstrument dec.svar) then
      Cil.SkipChildren
    else begin
      let parList = dec.sformals in
      List.iter (fun v ->
          self#fill_aux v.vid
        ) parList;
      Cil.DoChildren
    end

  method! vstmt_aux stmt =
    match stmt.skind with
    | Instr i when Utils.is_label i -> Cil.SkipChildren (* Ignorer les labels *)
    | Instr (Set ((Var v,_),_,_))
    | Instr (Call (Some (Var v,_),_,_,_))
    | Instr (Local_init (v,_,_)) ->
      Cil.DoChildrenPost (fun f ->
          if not (v.vname = "__retres") && not v.vtemp then begin
            self#fill_def v.vid
          end; f
        )
    | Loop (_,b,_,_,_) ->
      Stack.push stmt.sid inLoopId;
      ignore(Cil.visitCilBlock (self :> Cil.cilVisitor) b);
      ignore(Stack.pop inLoopId);
      Cil.SkipChildren
    | _ -> Cil.DoChildren
end

(** Used to sort combinations *)
exception Sorted

(** From previously found definitions, extract all combinations for
    each expression with more than 1 LVal *)
class computeCombinations = object(self)
  inherit Visitor.frama_c_inplace

  (** Stack that store loops id wen entering in it, and pop it when leaving *)
  val inLoopId = Stack.create ()

  method! vfunc dec =
    if not (Annotators.shouldInstrument dec.svar) then
      Cil.SkipChildren
    else begin
      let parList = dec.sformals in
      List.iter (fun v ->
          Hashtbl.replace currentDef v.vid ((Hashtbl.find currentDef v.vid) + 1);
        ) parList;
      Cil.DoChildren
    end

  (* Def-Var *)
  method! vstmt_aux stmt =
    match stmt.skind with
    | Instr i when Utils.is_label i -> Cil.SkipChildren (* Ignorer les labels *)
    | Instr (Set ((Var v,_),_,_))
    | Instr (Call (Some (Var v,_),_,_,_))
    | Instr (Local_init (v,_,_)) ->
      Cil.DoChildrenPost (fun f ->
          (* We just increase the current Definitions *)
          if not (v.vname = "__retres") && not v.vtemp then begin
            Hashtbl.replace currentDef v.vid ((Hashtbl.find currentDef v.vid) + 1);
            if not (Stack.is_empty inLoopId) then begin
              let lid = get_varLoop_id v.vid (Stack.top inLoopId) in
              Hashtbl.replace currentDef lid ((Hashtbl.find currentDef lid) + 1);
            end
          end; f
        )
    | Loop (_,b,_,_,_) ->
      Stack.push stmt.sid inLoopId;
      ignore(Cil.visitCilBlock (self :> Cil.cilVisitor) b);
      ignore(Stack.pop inLoopId);
      Cil.SkipChildren
    | _ -> Cil.DoChildren

  (** This method will, for each expression :
      1. Get its number of Lvals, if this number is > to 1 then procceed to next steps
      2. For each variable, get its current definition
      3. Unroll each variable, [(x,3);(y,1)] becames [[(x,1);(x,2);(x,3)];[(y,1)]]
         If x appears in a loop, then add it with an offset
      4. Calculate cartesian product size of this list of lists,
         if it's to big then warn the user and skip this expression
      5. Do the product, this will give us all combaniation possible of each variable definitions
      6. Sort all combination un lexicographical order, then for each combination
         fill all hashtbls
  *)
  method! vexpr expr =
    let vExp = new multiUseExp in
    (* 1 *)
    ignore(Cil.visitCilExpr (vExp :> Cil.cilVisitor) expr);
    let lvalIds = vExp#get_LVals () in
    if List.length lvalIds > 1 then begin
      (* 2 *)
      let alldefs = List.fold_left (fun acc id -> (id, (Hashtbl.find currentDef id) - 1) :: acc ) [] lvalIds in
      (* 3 *)
      let nl = List.fold_left (fun acc (vid,nbDef) ->
          let normalDefs =
            Array.to_list (Array.init nbDef (fun idDef -> (vid,idDef+1))) in
          let inLoopDefs =
            if not (Stack.is_empty inLoopId) && Hashtbl.mem varLoopID (vid,Stack.top inLoopId) then begin
              let lid = get_varLoop_id vid (Stack.top inLoopId) in
              let currDef = (Hashtbl.find currentDef lid) in
              let comingNext = (Hashtbl.find nBVarDefs lid) - currDef + 1 in
              let totalDef = Hashtbl.find nBVarDefs vid in
              Array.to_list (Array.init comingNext
                               (fun idDef -> (vid,totalDef+currDef+idDef)))
            end
            else
              []
          in
          (normalDefs@inLoopDefs) :: acc
        ) [] alldefs
      in
      (* 4 *)
      let taille_prod = List.fold_left (fun acc l -> acc * (List.length l)) 1 nl in
      if taille_prod <= Options.MaxContextPath.get () then begin
        (* 5 *)
        let all_cases = n_cartesian_product nl in
        let sort (l1:(int*int) list) (l2:(int*int) list) : int =
          let ret = ref 0 in
          try
            List.iter2 (fun (_,c1) (_,c2) ->
              ret := c1 - c2;
              if !ret != 0 then raise Sorted
              ) l1 l2; 0
          with Sorted -> !ret
        in
        let f (id:int) (cc: int*int) : unit =
          if Hashtbl.mem invertedPathID cc then
            Hashtbl.replace invertedPathID cc (id :: (Hashtbl.find invertedPathID cc))
          else
            Hashtbl.add invertedPathID cc [id]
        in
        (* 6 *)
        let all_cases = List.sort sort all_cases in
        List.iter (fun c ->
            (* Generate an unique id for this combination *)
            let new_id = Annotators.next () in
            (* Associates this id with the expression *)
            if Hashtbl.mem multiUses expr.eid then
              Hashtbl.replace multiUses expr.eid (new_id :: (Hashtbl.find multiUses expr.eid))
            else
              Hashtbl.add multiUses expr.eid [new_id];
            (* Associates this id to a couple Length/Position*)
            Hashtbl.add combsID new_id ((List.length c) +1,ref 1);
            (* For each element of this combination, store this combination id *)
            List.iter (f new_id) c
          ) all_cases
      end
      else begin
        Options.warning "Expression ignored in file %a, too many paths (%d)" Printer.pp_location expr.eloc taille_prod;
        ignoredLabels := (List.length lvalIds) * taille_prod + !ignoredLabels
      end
    end;
    Cil.SkipChildren
end

(** Visitor that will add all labels using the previously filled Hashtbl **)
class addLabels = object(self)
  inherit Visitor.frama_c_inplace

  (** Stack that store loops id wen entering in it, and pop it when leaving *)
  val inLoopId = Stack.create ()

  (** From a sequence id, a variable id, this sequence length and current position,
      create the corresponding label ad store it *)
  method private mkSeq (ids:int) (vid:string) (sid:int) (lid:int) : unit =
    let idExp = Exp.kinteger IULong ids in
    let oneExp = Exp.one () in
    let curr = Exp.integer sid in
    let slen = Exp.integer lid in
    let varExp = Exp.string vid in
    let zeroExp = Exp.zero () in
    let newStmt = Utils.mk_call "pc_label_sequence" ([oneExp;idExp;curr;slen;varExp;zeroExp]) in
    if sid != lid (*Def*) then
      labelDefs := newStmt :: !labelDefs
    else (* Use *)
      labelUses := newStmt :: !labelUses

  (** Create a pc_label_sequence_condiion and store it*)
  method private mkCond (vid:int) : unit =
    let zeroExp = Exp.zero () in
    let ccExp = Exp.string (string_of_int vid) in
    let newStmt = (Utils.mk_call "pc_label_sequence_condition" ([zeroExp;ccExp])) in
    labelStops := newStmt :: !labelStops

  method private handle_param v =
    let i = (Hashtbl.find currentDef v.vid) in
    if Hashtbl.mem invertedPathID (v.vid,i) then begin
      (* Je récupère toutes les expressions ou v apparait avec cette def *)
      let combIds = Hashtbl.find invertedPathID (v.vid,i) in
      Hashtbl.remove invertedPathID (v.vid,i);
      let f (idComb:int) : unit =
        let (len,sid) = Hashtbl.find combsID idComb in
        self#mkSeq idComb (string_of_int v.vid) !sid len;
        incr sid
      in
      List.iter f (List.rev combIds)
    end;
    Hashtbl.replace currentDef v.vid (i + 1);

  method! vfunc dec =
    if not (Annotators.shouldInstrument dec.svar) then
      Cil.SkipChildren
    else begin
      labelDefs := [];
      let parList = dec.sformals in
      List.iter self#handle_param parList;
      let labelParams = List.rev !labelDefs in
      labelDefs := [];
      Cil.DoChildrenPost (fun dec ->
          (dec.sbody.bstmts <- (labelParams @ dec.sbody.bstmts)); dec
        )
    end

  method! vstmt_aux stmt =
    let lbl = List.length stmt.labels != 0 in
    match stmt.skind with
    | Instr i when Utils.is_label i -> Cil.SkipChildren (* Ignore labels *)
    | Instr (Set ((Var v,_),_,_))
    | Instr (Call (Some (Var v,_),_,_,_))
    | Instr (Local_init (v,_,_)) ->
      let processSet v =
        let vid = v.vid in
        if not (v.vname = "__retres") && not v.vtemp then begin
          self#mkCond vid;
          let currDef = Hashtbl.find currentDef vid in
          if Hashtbl.mem invertedPathID (vid,currDef) then begin
            (* Get all combinations where v appears with its current def *)
            let combIds = Hashtbl.find invertedPathID (vid,currDef) in
            Hashtbl.remove invertedPathID (vid,currDef);
            (* For each combination, create a sequence label and increment the position*)
            let f idComb =
              let (len,sid) = Hashtbl.find combsID idComb in
              self#mkSeq idComb (string_of_int vid) !sid len;
              incr sid
            in
            List.iter f combIds;
          end;
          Hashtbl.replace currentDef vid (currDef + 1);

          if not (Stack.is_empty inLoopId) then begin
            let lid = Stack.top inLoopId in
            let nvid = get_varLoop_id vid lid in
            let lcurrDef = Hashtbl.find currentDef nvid in
            let oftDef = Hashtbl.find nBVarDefs vid + lcurrDef in
            if Hashtbl.mem invertedPathID (vid,oftDef) then begin
              (* Same thing but in loop *)
              let combIds = Hashtbl.find invertedPathID (vid,oftDef) in
              Hashtbl.remove invertedPathID (vid,oftDef);
              let f idComb =
                let (len,sid) = Hashtbl.find combsID idComb in
                self#mkSeq idComb (string_of_int vid) !sid len;
                incr sid
              in
              List.iter f combIds
            end;
            Hashtbl.replace currentDef nvid (lcurrDef + 1)
          end;
        end
      in
      Cil.DoChildrenPost (fun stmt ->
          processSet v;
          let res =
            if not lbl then
              Stmt.block (!labelUses @ !labelStops @ [stmt] @ !labelDefs)
            else
              Stmt.block ({stmt with skind = Block (Block.mk (!labelUses @ !labelStops @ [Stmt.mk stmt.skind]))} :: !labelDefs)
          in
          labelUses := [];
          labelDefs := [];
          labelStops := [];
          res
        )
    | If (ex,th,el,lo) ->
      ignore(Cil.visitCilExpr (self :> Cil.cilVisitor) ex);
      (let lu = !labelUses in labelUses := [];
       let thenb = (Cil.visitCilBlock (self :> Cil.cilVisitor) th) in
       let elseb = (Cil.visitCilBlock (self :> Cil.cilVisitor) el) in
       let newSt = (Block.mk (lu @ [Stmt.mk (If (ex,thenb,elseb,lo))])) in stmt.skind <- (Block newSt);
       Cil.ChangeTo stmt)
    | Switch (ex, b, stmtl, lo) ->
      ignore(Cil.visitCilExpr (self :> Cil.cilVisitor) ex);
      (let lu = !labelUses in labelUses := [];
       let nb = (Cil.visitCilBlock (self :> Cil.cilVisitor) b) in
       let newSt = (Block.mk (lu @ [Stmt.mk (Switch (ex,nb,stmtl,lo))])) in stmt.skind <- (Block newSt);
       Cil.ChangeTo stmt)
    | Loop (_,b,_,_,_) ->
      Stack.push stmt.sid inLoopId;
      ignore(Cil.visitCilBlock (self :> Cil.cilVisitor) b);
      ignore(Stack.pop inLoopId);
      Cil.SkipChildren
    | _ ->
      Cil.DoChildrenPost (fun stmt ->
          let res =
            if not lbl then
              Stmt.block (!labelUses @ [stmt])
            else begin
              stmt.skind <-Block (Block.mk (!labelUses @ [Stmt.mk stmt.skind]));
              stmt
            end
          in
          labelUses := []; res
        )

  (* Use *)
  method! vexpr expr =
    let eid = expr.eid in
    if Hashtbl.mem multiUses eid then begin
      (* For each combination of this expr, and the last sequence *)
      let combIds = Hashtbl.find multiUses eid in
      let f idComb =
        let (len,_) = Hashtbl.find combsID idComb in
        self#mkSeq idComb "N/A" len len;
        totalLabels := len + !totalLabels;
        idList := (eid,idComb) :: !idList
      in
      List.iter f combIds
    end;
    Cil.SkipChildren
end

(** Store all combination's ids in a hashtbl grouped by expr id, then generate hyperlabels *)
let compute_hl () =
  let regroup = Hashtbl.create 512 in
  let fill (eid,idComb) =
    if Hashtbl.mem regroup eid then
      Hashtbl.replace regroup eid ((Hashtbl.find regroup eid)@[idComb])
    else
      Hashtbl.add regroup eid [idComb]
  in
  List.iter fill !idList;
  Hashtbl.fold (fun _ seqs str ->
      List.fold_left (fun acc s -> acc ^ "<s" ^ string_of_int s ^"|; ;>,\n" ) str seqs
    ) regroup ""

let gen_hyperlabels () =
  let data_filename = (Filename.chop_extension (Annotators.get_file_name ())) ^ ".hyperlabels" in
  Options.feedback "write hyperlabel data (to %s)" data_filename;
  let data = compute_hl () in
  let out = open_out_gen [Open_creat; Open_append] 0o640 data_filename in
  output_string out data;
  close_out out;
  Options.feedback "Total number of labels = %d" !totalLabels;
  Options.feedback "Total number of ignored labels = %d" !ignoredLabels


(** Successively pass the 3 visitors, reset curentDef after the 2nd one *)
let visite file =
  Visitor.visitFramacFileSameGlobals (new countDef :> Visitor.frama_c_visitor) file;
  Visitor.visitFramacFileSameGlobals (new computeCombinations :> Visitor.frama_c_visitor) file;
  Hashtbl.iter (fun k _ -> Hashtbl.replace currentDef k 1) currentDef;
  Visitor.visitFramacFileSameGlobals (new addLabels :> Visitor.frama_c_visitor) file

(**
   Context criteria annotator
*)
module Context = Annotators.Register (struct
    let name = "context"
    let help = "Context Coverage"
    let apply _ file =
      Options.result "[WIP] Context is currently in Alpha";
      visite file;
      gen_hyperlabels ()
  end)
