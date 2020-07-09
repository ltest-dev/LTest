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
open Ast_const

(* Main type used for dataflow analysis *)
type def = {
  (* Unique ID for each def, in increasing order. *)
  id: int;
  (* (function id) if this def is from a function's formals, -1 otherwise. *)
  funId: int;
  (* id of the variable being defined. *)
  varId_def: int;
  (* Current definition of the variable (number of def seen before + 1). *)
  defId: int;
  (* Statement id, will be use to "add" the sequence to its stmt at the end. *)
  stmtDef: int;
}

(* Main type used for dataflow analysis *)
type use = {
  varId_use: int;
  stmtUse: stmt;
}

(* Main type used for our analysis's state *)
module DefSet = Set.Make (
  struct type t = def
    let compare d1 d2 = compare d1.id d2.id
  end)

module UseSet = Set.Make (
  struct type t = use
    let compare u1 u2 =
      let tmp = compare u1.varId_use u2.varId_use in
      if tmp = 0 then Cil_datatype.Stmt.compare u1.stmtUse u2.stmtUse
      else tmp
  end)

(* Represent states in our analysis*)
type t =
  | Bottom
  | NonBottom of (DefSet.t * UseSet.t)

(* Count the number of sequences. *)
let nb_seqs = ref 0

(* Def's ID, used to know the order of seen def during dataflow analysis. *)
let count_def = ref 1

(* For each variable, keep the number of assignment seen for this variable
   key : variable id
   value : def counter
*)
let def_id : (int, int) Hashtbl.t = Hashtbl.create 32

(* Used to assign unique id for specific variable and field of structs
   key : variable id * field name
   value : unique id
*)
let offset_id : (int*string, int) Hashtbl.t = Hashtbl.create 32

(* each def is registered to avoid creating them more than once
   key : statement id
   value : def
*)
let seen_def : (int,def) Hashtbl.t = Hashtbl.create 32

(* bind each new sequence (def) to its corresponding statement, sequence id is
   used to sort them at the end
   key : statement id
   value : (sequence id * statement) list
*)
let to_add_defs : (int, (int*stmt) list) Hashtbl.t = Hashtbl.create 32
(* bind each new sequence (use)  to its corresponding statement *)
let to_add_uses : (int, (int*stmt) list) Hashtbl.t = Hashtbl.create 32
(* bind each new sequence (cond) to its corresponding statement *)
let to_add_cond : (int, stmt) Hashtbl.t = Hashtbl.create 32
(* bind each new sequence (def) to its corresponding function (for formals)
   key : function id
   value : (sequence id * statement) list
*)
let to_add_fun : (int, (int*stmt) list) Hashtbl.t = Hashtbl.create 32

(* Used to create hyperlabels
   key : variable id * definition id
   value : (sequence id) list
*)
let hyperlabels : (int*int, int list) Hashtbl.t = Hashtbl.create 32

(* Increments count_def and returns the old value *)
let next () =
  let tmp = !count_def in
  incr count_def;
  tmp

let print_def elt =
  Printf.printf "funId: %d / varId: %d / defId: %d / stmtDef: %d\n%!"
    elt.funId elt.varId_def elt.defId elt.stmtDef

let print_use elt =
  Printf.printf "varId: %d / sid: %d\n%!"
    elt.varId_use elt.stmtUse.sid

let print_uses set =
  Printf.printf "Uses : @.";
  UseSet.iter (fun elt -> Printf.printf "    @."; print_use elt) set

let print_defs set =
  Printf.printf "Defs : @.";
  DefSet.iter (fun elt -> Printf.printf "    @."; print_def elt) set

let pretty _ = function
  | Bottom -> Printf.printf "Bottom\n@."
  | NonBottom (defs,uses) ->
    Printf.printf "NonBottom : \n@.";
    print_defs defs;
    print_uses uses

(* Clear all hashtbl and reset counter after each function in addSequences *)
let reset_all () : unit =
  Hashtbl.reset to_add_defs;
  Hashtbl.reset to_add_uses;
  Hashtbl.reset to_add_cond;
  Hashtbl.reset to_add_fun;
  Hashtbl.reset def_id;
  Hashtbl.reset offset_id;
  Hashtbl.reset seen_def;
  count_def := 0

(* generic function for Hashtbl which use list as values.
   If the binding key exists, adds elt at the beginning of the list,
   else create a new binding. *)
let replace_or_add_list_front (tbl:('a,'b list) Hashtbl.t) (key:'a) (elt:'b) =
  if Hashtbl.mem tbl key then begin
    let old = Hashtbl.find tbl key in
    Hashtbl.replace tbl key (elt::old)
  end
  else
    Hashtbl.add tbl key [elt]

(* generic function for Hashtbl which use list as values.
   If the binding key exists, adds elt at the end of the list,
   else create a new binding. *)
let replace_or_add_list_back (tbl:('a,'b list) Hashtbl.t) (key:'a) (elt:'b) =
  if Hashtbl.mem tbl key then begin
    let old = Hashtbl.find tbl key in
    Hashtbl.replace tbl key (old@[elt])
  end
  else
    Hashtbl.add tbl key [elt]

(* Create a new def. *)
let make_def ?(funId = (-1)) (varId_def: int) (defId: int) (stmtDef: int) : def =
  {id=next();funId;varId_def;defId;stmtDef}

(* Add a sequence id to its corresponding hyperlabel *)
let add_to_hyperlabel (key:int*int) (ids:int) : unit =
  replace_or_add_list_front hyperlabels key ids

(* Given a variable id, increment the number of defs seen
   and returns it. If it is the first return 1. *)
let get_next_def_id (vid:int) : int  =
  if Hashtbl.mem def_id vid then begin
    let n = (Hashtbl.find def_id vid) + 1 in
    Hashtbl.replace def_id vid n;
    n
  end
  else
    (Hashtbl.add def_id vid 1; 1)

(* For a given vid and field name, returns their unique id *)
let get_offset_id (vid:int) (field_name:string) : int =
  if Hashtbl.mem offset_id (vid,field_name) then
    Hashtbl.find offset_id (vid,field_name)
  else begin
    let new_id = Cil_const.new_raw_id () in
    Hashtbl.add offset_id (vid,field_name) new_id;
    new_id
  end

(* Recursively create new unique id for each field used with the variable.
   Exemple :
   A_struct.field1.field2id will be :
   create new_vid  for (A_struct.vid, field1)
   create new_vid2 for (new_vid     ,field2)
   returns new_vid2
*)
let rec get_vid_with_field (vid:int) (offset:offset) : int =
  if Options.HandleStruct.get () then
    match offset with
    | Field (f_info,offset') ->
      let new_id = get_offset_id vid f_info.fname in
      get_vid_with_field new_id offset'
    | _ -> vid
  else
    vid

(* Try to find a use in state wich dom/post-dom current use  *)
let is_equivalent vid stmt kf uses =
  UseSet.exists (fun use ->
      use.varId_use = vid &&
      Dominators.dominates use.stmtUse stmt &&
      !Db.Postdominators.is_postdominator
        kf ~opening:use.stmtUse ~closing:stmt
    ) uses

(* Annot use in expression only once  *)
let is_triv_equiv vid visited =
  Options.CleanEquiv.get() &&
  List.exists (fun vid' -> vid' = vid) visited

(* Test if a LVal should be instrumented, i.e. it's not a global,
   a temp variable or if it's the first time we see it in the current
   expr/instr (except if CleanDuplicate is true) *)
let should_instrument (v:varinfo) vid visited : bool =
  not v.vglob && not (v.vname = "__retres") && not v.vtemp
  && not (is_triv_equiv vid visited)

(***********************************)
(********* Defuse Criteria *********)
(***********************************)

(* This visitor visits each LVal to create sequences if the state t contains
   defs for theses LVals *)
class visit_defuse ((defs_set,uses_set):DefSet.t*UseSet.t) current_stmt kf = object(self)
  inherit Visitor.frama_c_inplace
  val mutable visited = []

  (* Create a sequence member
     ids : sequence id
     vid : variable id
     id : position of this member (ex. 1=def and 2=use for def-use pairs
     max : size of the sequence (2 for def-use pairs)
  *)
  method private mkSeq_aux (ids: int) (vid: string) (id:int) (max:int) : stmt =
    let idExp = Exp.kinteger IULong ids in
    let oneExp = Exp.one () in
    let curr = Exp.integer id in
    let slen = Exp.integer max in
    let varExp = Exp.string vid in
    let zeroExp = Exp.zero () in
    Utils.mk_call "pc_label_sequence" ([oneExp;idExp;curr;slen;varExp;zeroExp])

  (* Create a condition which will break sequences for the variable vid *)
  method private  mkCond (vid: int) : stmt =
    let zeroExp = Exp.zero () in
    let ccExp = Exp.string (string_of_int vid) in
    Utils.mk_call "pc_label_sequence_condition" ([zeroExp;ccExp])

  (* Create a def-use sequence for the given def *)
  method private mkSeq (def:def) : unit =
    let ids = Annotators.next () in (* sequence id *)
    let sdef = self#mkSeq_aux ids (string_of_int def.varId_def) 1 2 in (* def part *)
    let suse = self#mkSeq_aux ids (string_of_int def.varId_def) 2 2 in (* use part *)
    (* Add sdef to either defs table or function table *)
    if def.funId = -1 then begin
      (* Add a cond label for this def *)
      if not (Hashtbl.mem to_add_cond def.stmtDef) then
        Hashtbl.add to_add_cond def.stmtDef (self#mkCond def.varId_def);
      replace_or_add_list_front to_add_defs def.stmtDef (ids,sdef)
    end
    else
      replace_or_add_list_front to_add_fun def.funId (ids,sdef);
    (* Add the use *)
    replace_or_add_list_front to_add_uses current_stmt.sid (ids,suse);
    (* Register this sequence to its hyperlabel *)
    add_to_hyperlabel (def.varId_def, def.defId) ids;
    incr nb_seqs

  (* Visit all expressions and sub expressions to find lvals *)
  method! vexpr expr =
    match expr.enode with
    | Lval (Var v, offset) ->
      let vid = get_vid_with_field v.vid offset in
      (* Keeps defs related to this variable *)
      let all_vid_defs = DefSet.filter (fun def -> def.varId_def = vid) defs_set in
      if should_instrument v vid visited then begin
        visited <- vid :: visited;
        if not (DefSet.is_empty all_vid_defs) then
          if not (Options.CleanEquiv.get ())
          || not (is_equivalent vid current_stmt kf uses_set) then
            DefSet.iter self#mkSeq all_vid_defs;
      end;
      Cil.DoChildren
    | _ -> Cil.DoChildren
end


(******************************)
(***** Dataflow analysis ******)
(******************************)
class visit_use state stmt = object(_)
  inherit Visitor.frama_c_inplace
  val mutable visited = []

  (* Visit all expressions and sub expressions to find lvals *)
  method! vexpr expr =
    match !state with
    | Bottom -> Cil.SkipChildren
    | NonBottom (defs,uses) ->
      match expr.enode with
      | Lval (Var v, offset) ->
        let vid = get_vid_with_field v.vid offset in
        if should_instrument v vid visited then begin
          visited <- vid :: visited;
          let new_uses = UseSet.add {varId_use=vid;stmtUse=stmt} uses in
          state := NonBottom (defs, new_uses);
        end;
        Cil.DoChildren
      | _ -> Cil.DoChildren
end


module P() = struct

  let pretty = pretty

  (* Return a new set after removing all definitions of a variable *)
  let remove_def vid s =
    DefSet.filter (fun d -> d.varId_def <> vid) s

  (* Return a new set after removing all definitions of a variable *)
  let remove_use vid s =
    UseSet.filter (fun u -> u.varId_use <> vid) s

  type nonrec t = t

  (* Function called to join 2 states *)
  let join a b =
    match a,b with
    | Bottom, x | x, Bottom -> x
    | NonBottom (d,u), NonBottom (d',u') ->
      NonBottom (DefSet.union d d',UseSet.inter u u')

  (* is the set a is a subset of b*)
  let is_included a b =
    match a,b with
    | Bottom, _ -> true
    | NonBottom _, Bottom -> false
    | NonBottom (d,u), NonBottom (d',u') ->
      DefSet.subset d d' && UseSet.subset u u'

  let join_and_is_included a b =
    join a b, is_included a b

  let bottom = Bottom

  (* For each definition statement, change the current state by
     adding or not new definition, removing older ones etc... *)
  let do_def vid offset sid = function
    | Bottom -> Bottom
    | NonBottom (d,u) ->
      let vid = get_vid_with_field vid offset in
      let d_clean =
        if Options.CleanDataflow.get () then
          remove_def vid d
        else
          d
      in
      let u_clean = remove_use vid u in
      let new_d =
        if Hashtbl.mem seen_def sid then
          DefSet.add (Hashtbl.find seen_def sid) d_clean
        else begin
          let defId = get_next_def_id vid in
          let new_def = make_def vid defId sid in
          Hashtbl.add seen_def sid new_def;
          DefSet.add new_def d_clean
        end
      in
      NonBottom (new_d,u_clean)

  (* Function called for each stmt and propagating new states to each succs of stmt *)
  let transfer_stmt stmt state =
    match stmt.skind with
    | Instr i when not (Utils.is_label i) ->
      let state = ref state in
      ignore(Cil.visitCilInstr  (new visit_use state stmt :> Cil.cilVisitor) i);
      begin match i with
        | Set ((Var v,offset),_,_)
        | Call (Some (Var v,offset),_,_,_) ->
          if not (v.vname = "__retres") && not v.vtemp then begin
            let res = do_def v.vid offset stmt.sid !state in
            List.map (fun x -> (x,res)) stmt.succs
          end
          else List.map (fun x -> (x,!state)) stmt.succs
        | Local_init (v,_,_) ->
          if not (v.vname = "__retres") && not v.vtemp then begin
            let res = do_def v.vid NoOffset stmt.sid !state in
            List.map (fun x -> (x,res)) stmt.succs
          end
          else List.map (fun x -> (x,!state)) stmt.succs
        | _ ->
          List.map (fun x -> (x,!state)) stmt.succs
      end
    | Return (Some e,_)
    | If (e,_,_,_)
    | Switch (e,_,_,_) ->
      let state = ref state in
      ignore(Cil.visitCilExpr  (new visit_use state stmt :> Cil.cilVisitor) e);
      List.map (fun x -> (x,!state)) stmt.succs
    | _ -> List.map (fun x -> (x,state)) stmt.succs

end

(* For each function, do a dataflow analysis and create sequences
   Initial state contains def of each formal
*)
let do_function kf =
  let module Fenv = (val Dataflows.function_env kf) in
  let module Inst = P() in
  let args = Kernel_function.get_formals kf in
  let first_stmt = Kernel_function.find_first_stmt kf in
  let funId = Kernel_function.get_id kf in
  let f acc arg =
    let defId = get_next_def_id arg.vid in
    DefSet.add (make_def ~funId arg.vid defId first_stmt.sid) acc
  in
  let init_d = List.fold_left f DefSet.empty args in
  let module Arg = struct
    include Inst
    let init =
      [(first_stmt, NonBottom (init_d, UseSet.empty))]

  end in
  let module Results = Dataflows.Simple_forward(Fenv)(Arg) in
  let f stmt state =
    match state with
    | Bottom -> ()
    | NonBottom t ->
      begin
        match stmt.skind with
        | Instr i when not (Utils.is_label i) ->
          ignore(Cil.visitCilInstr (new visit_defuse t stmt kf :> Cil.cilVisitor) i);
        | Return (Some e,_)
        | If (e,_,_,_)
        | Switch (e,_,_,_) ->
          ignore(Cil.visitCilExpr  (new visit_defuse t stmt kf :> Cil.cilVisitor) e);
        | _ -> ()
      end
  in
  Results.iter_on_result f

(* This visitor will, for each function :
   - Do the dataflows analysis and fill hashtbls
   - Then visit the body of the function, add add all sequences to where
     they belong.
   - Clean all hashtbls and do the next function (if any)
*)
class addSequences = object(self)
  inherit Visitor.frama_c_inplace

  (* get all sequences, sort defs and uses by sequence ID, and returns
     a pair of list (before,after) with sequences to add before & after the current statement *)
  method private get_seqs_sorted sid =
    let defs =
      if Hashtbl.mem to_add_defs sid then
        Hashtbl.find to_add_defs sid
      else []
    in
    let uses =
      if Hashtbl.mem to_add_uses sid then
        Hashtbl.find to_add_uses sid
      else []
    in
    let cond =
      if Hashtbl.mem to_add_cond sid then
        [Hashtbl.find to_add_cond sid]
      else []
    in
    let compare (id1,_) (id2,_) = compare id1 id2 in
    let defs,uses = List.sort compare defs, List.sort compare uses in
    List.map (fun (_,s) -> s) defs, (List.map (fun (_,s) -> s) uses) @ cond

  method! vfunc (dec : fundec) : fundec Cil.visitAction =
    let kf = Extlib.the self#current_kf in
    if Kernel_function.is_definition kf && Annotators.shouldInstrument dec.svar then begin
      Cfg.clearCFGinfo ~clear_id:false dec;
      Cfg.cfgFun dec;
      do_function kf;
      Cil.DoChildrenPost (fun f ->
          let id = dec.svar.vid in
          let defs = List.sort compare (
              if Hashtbl.mem to_add_fun id then
                Hashtbl.find to_add_fun id
              else [])
          in
          let params = List.map (fun (_,s) -> s) defs in
          f.sbody.bstmts <- params @ f.sbody.bstmts;
          reset_all ();
          f
        )
    end
    else Cil.SkipChildren

  method! vblock _ =
    Cil.DoChildrenPost (fun block ->
        let rec aux l acc =
          match l with
          | [] -> acc
          | s :: t ->
            let after,before = self#get_seqs_sorted s.sid in
            (* if the statement has 1 or more labels, then moves it to
               the first statement of before if it exists *)

            if s.labels <> [] && before <> [] then begin
              s.skind <- Block (Block.mk (before @ [Stmt.mk s.skind]));
              aux t (acc @ [s] @ after)
            end
            else
              aux t (acc @ before @ [s] @ after)
        in block.bstmts <- aux block.bstmts [];
        block
      )

  method! vstmt s =
    match s.skind with
    | UnspecifiedSequence v ->
      s.skind <- Block (Cil.block_from_unspecified_sequence v); Cil.DoChildren
    | _ -> Cil.DoChildren

end

(** Hyperlabel's type *)
let symb : string ref = ref "."

(** Create all hyperlabels *)
let compute_hl () : string =
  if "-" = !symb then
    Hashtbl.fold (fun _ seqs str ->
        let seqs = List.sort compare seqs in
        List.fold_left (fun acc s -> acc ^ Annotators.next_hl() ^ ") <s" ^ string_of_int s ^"|; ;>,\n") str seqs
      ) hyperlabels ""
  else
    Hashtbl.fold (fun _ seqs str ->
        let seqs = List.sort compare seqs in
        str ^ Annotators.next_hl() ^ ") <" ^ (String.concat !symb (List.map (fun s -> "s" ^ string_of_int s) seqs)) ^ "|; ;>,\n"
      ) hyperlabels ""

let gen_hyperlabels () =
  let data_filename = (Filename.chop_extension (Annotators.get_file_name ())) ^ ".hyperlabels" in
  Options.feedback "write hyperlabel data (to %s)" data_filename;
  let data = compute_hl () in
  let out = open_out_gen [Open_creat; Open_append] 0o644 data_filename in
  output_string out data;
  close_out out;
  Options.feedback "Total number of sequences = %d" !nb_seqs;
  Options.feedback "Total number of hyperlabels = %d" (Annotators.getCurrentHLId())

(** Successively pass the 2 visitors *)
let visite (file : file) : unit =
  Visitor.visitFramacFileSameGlobals (new addSequences :> Visitor.frama_c_visitor) file;
  Cfg.clearFileCFG ~clear_id:false file;
  Cfg.computeFileCFG file;
  Ast.mark_as_changed ();

  (** All-defs annotator *)
module ADC = Annotators.Register (struct
    let name = "ADC"
    let help = "All-Definitions Coverage"
    let apply _ file =
      visite file;
      symb := "+";
      gen_hyperlabels ()
  end)

(** All-uses annotator *)
module AUC = Annotators.Register (struct
    let name = "AUC"
    let help = "All-Uses Coverage"
    let apply _ file =
      visite file;
      symb := ".";
      gen_hyperlabels ()
  end)

(** Def-Use annotator *)
module DUC = Annotators.Register (struct
    let name = "DUC"
    let help = "Definition-Use Coverage"
    let apply _ file =
      visite file;
      symb := "-";
      gen_hyperlabels ()
  end)
