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

let label_function_name = "__PC__LABEL"

(** Checks if the provided expression is a string constant *)
let rec cil_isString exp =
  match exp.enode with
  | Const (CStr str) -> Some str
  | CastE (_, exp) -> cil_isString exp
  | _ -> None

let to_lval_exp varinfo =  (* transform a varinfo into expression *)
  Cil.dummy_exp (Lval (Cil.var varinfo))

let location_string loc =
  let line = (fst loc).Lexing.pos_lnum in
  let file = Filename.basename (fst loc).Lexing.pos_fname in
  file^":"^string_of_int line

let rec last_element list =
  match list with
  | [] -> None
  | [e] -> Some(e)
  | _::t -> last_element t

let dump_ast ?file ?name () =
  let file = match file with Some file -> file | None -> Ast.get () in
  let name =
    match name with
    | Some name -> name
    | None ->
      let project = Project.get_name (Project.current ()) in
      ".luncov_dump_"^project^".c"
  in
  let out = open_out name in
  let f = Format.formatter_of_out_channel out in
  Printer.pp_file f file;
  Format.pp_print_flush f ();
  close_out out


let suceed_or_dump_ast ?file ?name f arg =
  try
    f arg
  with e ->
    let file = match file with Some file -> file | None -> Ast.get () in
    let name =
      match name with
      | Some name -> name
      | None ->
        let project = Project.get_name (Project.current ()) in
        ".luncov_dump_"^project^".c"
    in
    at_exit (fun () -> dump_ast ~file ~name ());
    raise e


type 'a okko = Ok of 'a | Ko of exn

let with_project ?selection prj (f : 'a -> 'b) (arg : 'a) : 'b=
  let g () =
    try
      Ok (f arg)
    with e -> Ko e
  in
  match Project.on ?selection prj g () with
  | Ko e -> raise e
  | Ok res -> res


let is_label_stmt stmt lblid =
  match stmt.skind with
  | Instr (Call (None, {enode=(Lval (Var {vname=name}, NoOffset))}, idexp::_::tagexp::_, _))
    when name = label_function_name ->
    begin
      match Cil.isInteger idexp, cil_isString tagexp  with
      | Some id, Some _ ->
        if Integer.to_int id == lblid then
          true, true
        else
          true, false
      | _ -> false, false
    end
  | _ -> false, false


let copy_parameters ?src dst =
  let wp_parameters = [
    "-wp-model";
    "-wp-out";
    "-wp-rte";
    "-wp-extensional";
    "-wp-extern-arrays";
    "-wp-literals";
    "-wp-prover";
    "-wp-steps";
    "-wp-timeout";
    "-wp-proof-trace";
    "-wp-par";
    "-wp-print";
  ] in
  let states = List.map Dynamic.Parameter.get_state wp_parameters in
  Project.copy ~selection:(State_selection.of_list states) ?src dst


class variable_harvester exprs =
  object
    inherit Visitor.frama_c_inplace

    method! vexpr e =
      match e.enode with
      | Lval _ | AddrOf _ | StartOf _ ->
        exprs := Cil_datatype.ExpStructEq.Set.add e !exprs;
        Cil.SkipChildren
      | _ ->
        Cil.DoChildren

    method! vstmt_aux s =
      match s.skind with
      | Instr _ (*Voir pour skip children en cas de Call*) (*MD: qu'est-ce que ça veut dire?*)
      | Return _
      | If _ | Switch _ -> (* the actual bodies of if/switch are not visited, see vblock*)
        Cil.DoChildren
      | _ -> Cil.SkipChildren

    method! vblock _ =
      Cil.SkipChildren
  end


let collect_variables stmt =
  let vars = ref Cil_datatype.ExpStructEq.Set.empty in
  let _ = Visitor.visitFramacStmt (new variable_harvester vars) stmt in
  !vars

let collect_fun_param fdec =
  List.fold_left (fun acc v -> Cil_datatype.ExpStructEq.Set.add (to_lval_exp v) acc) Cil_datatype.ExpStructEq.Set.empty fdec.sformals

(* Create a predicate from an operator, an expression, and an integer *)
let const_to_pred op exp cst =
  let var = Logic_utils.expr_to_term ~cast:false exp in
  let value = Cil.lconstant cst in
  Logic_const.prel (op, var, value)

(* Create a predicate asserting the interval of some expression *)
let interval_to_exp ~about min max =
  if Extlib.has_some min then
    let rmin =  const_to_pred Rge about (Extlib.the min) in
    if Extlib.has_some max then
      let rmax = const_to_pred Rle about (Extlib.the max) in
      Some (Logic_const.pands [rmin;rmax])
    else
      Some rmin
  else
  if Extlib.has_some max then
    Some (const_to_pred Rle about (Extlib.the max))
  else
    None

let interval_to_congruence_exp lv min max rem modulo = (* convertit interval+ congruence en prédicat *)
  let res = interval_to_exp lv min max in
  if Integer.equal modulo Integer.one then
    res
  else
    let var = Logic_utils.expr_to_term true lv in
    let cst_mod = Cil.lconstant modulo in
    let cst_rem = Cil.lconstant rem in
    let ltyp = Logic_utils.typ_to_logic_type (Cil.typeOf lv) in
    let left = Logic_const.term (TBinOp (Mod, var, cst_mod)) ltyp in
    let congru_assert =  Logic_const.prel (Req, left, cst_rem) in
    if Extlib.has_some res then
      Some (Logic_const.pands [(Extlib.the res);congru_assert])
    else
      Some congru_assert

let max_enumerate = 8

(** Create a predicate from an interval ([ival]) about some expression *)
let pred_of_ival ~about:lve iv =
  if Ival.is_bottom iv then
    None (* Should be FALSE ?? *)
  else if Ival.is_singleton_int iv then
    Some (const_to_pred Req lve (Ival.project_int iv))
  else
    match iv with
    | Ival.Set arr when Array.length arr <= max_enumerate ->
      let predarr = Array.map (fun v -> const_to_pred Req lve v) arr in
      Some (Logic_const.pors (Array.to_list predarr))
    | Ival.Set _ -> interval_to_exp lve (Ival.min_int iv) (Ival.max_int iv) (* si c'est un set simplifie avec un intervale *)
    | Ival.Top (min,max,rem,modulo) -> interval_to_congruence_exp lve min max rem modulo
    | _ -> None

exception Project_Ival_Fail
exception Project_Ival_Unreachable

let ival_of_exp ~at:stmt expr =
  assert (Db.Value.is_computed ());
  let state = Db.Value.get_stmt_state stmt in
  (* first check if the state is actually reachable *)
  if Db.Value.is_reachable state then
    let loc_bytes = !Db.Value.eval_expr ~with_alarms:CilE.warn_none_mode state expr in
    try
      let iv = Cvalue.V.project_ival loc_bytes in
      iv
    with Cvalue.V.Not_based_on_null ->
      raise Project_Ival_Fail
  else
    raise Project_Ival_Unreachable

let exp_to_pred ~at expr =
  try
    let ival = ival_of_exp ~at expr in
    let pred = pred_of_ival expr ival in
    begin
      match pred with
      | Some p ->
        Options.debug "Projecting %a on sid:%d → Ok (%a)" Printer.pp_exp expr at.sid Printer.pp_predicate_node p.pred_content
      | None ->
        Options.debug "Projecting %a on sid:%d →  Ok (None)" Printer.pp_exp expr at.sid
    end;
    pred
  with
  | Project_Ival_Fail ->
    Options.debug "Projecting: %a on sid:%d → failed" Printer.pp_exp expr at.sid;
    None
  | Project_Ival_Unreachable ->
    Options.debug "Can't get %a value in stmt:[%d] code unreachable" Printer.pp_exp expr at.sid;
    None
