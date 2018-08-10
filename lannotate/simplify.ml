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

let debug_level = 3

type formula = [
  | `TAnd of formula * formula
  | `TOr of formula * formula
  | `TNot of formula
  | `TAtom of int
  | `TTrue
  | `TFalse
]

type formula2 = [
  | `TAnd of formula2 * formula2
  | `TOr of formula2 * formula2
  | `TAtomPos of int
  | `TAtomNeg of int
  | `TTrue
  | `TFalse
]

let rec pp_formula f x =
  match x with
  | `TAnd (a, b) ->
    Format.fprintf f "( %a /\\ %a )" pp_formula a pp_formula b
  | `TOr (a, b) ->
    Format.fprintf f "( %a \\/ %a )" pp_formula a pp_formula b
  | `TAtomPos i
  | `TAtom i ->
    Format.fprintf f "#%d" i
  | `TAtomNeg i ->
    Format.fprintf f "(~ #%d)" i
  | `TNot a ->
    Format.fprintf f "(~ %a)" pp_formula a
  | `TTrue ->
    Format.fprintf f "True"
  | `TFalse ->
    Format.fprintf f "False"

let rec ntimes n e =
  if n > 0 then e :: (ntimes (n-1) e)
  else []

let dnf_true n =
  [ ntimes n `Dontcare ]

let dnf_false (_n : int) =
  []

let dnf_or (n : int) a b =
  assert (List.for_all (fun minterm -> n = List.length minterm) a);
  assert (List.for_all (fun minterm -> n = List.length minterm) b);
  List.append a b

let dnf_and_extbools a b =
  match a, b with
  | `False, `True | `True, `False -> None
  | `True, (`Dontcare | `True) | `Dontcare, `True -> Some `True
  | `False, (`Dontcare| `False) | `Dontcare, `False -> Some `False
  | `Dontcare, `Dontcare -> Some `Dontcare

let dnf_and_minterms n aminterm bminterm =
  assert (n = List.length aminterm);
  assert (n = List.length bminterm);
  let f acc a b =
    match acc with
    | None -> None
    | Some acc ->
      match dnf_and_extbools a b with
      | Some res -> Some (res :: acc)
      | _ -> None
  in
  match List.fold_left2 f (Some []) aminterm bminterm with
  | Some res -> Some (List.rev res)
  | None -> None;;

let dnf_and_dnf_minterm (n: int) a bminterm =
  let f acc aminterm =
    match dnf_and_minterms n aminterm bminterm with
    | Some res -> res :: acc
    | None -> acc
  in
  let res = List.rev (List.fold_left f [] a) in
  Options.debug ~level:10 "@[%a@] . @[%a@] = @[%a@]"
    Bes.pp_dnf_expression a Bes.pp_dnf_minterm bminterm Bes.pp_dnf_expression res;
  res

let dnf_and (n: int) a b =
  let f acc minterm =
    dnf_or n (dnf_and_dnf_minterm n a minterm) acc
  in
  List.fold_left f [] b

let dnf_var (n: int) v value : [> `True |`False |`Dontcare] list list =
  assert (v >= 0 && v < n);
  [ (ntimes v `Dontcare) @ [ value ] @ (ntimes (n-v-1) `Dontcare) ]

let rec propagate_nots_aux ~neg (t : formula) : formula2 =
  match t, neg with
  | `TAnd (a, b), false ->
    `TAnd (propagate_nots_aux ~neg a, propagate_nots_aux ~neg b)
  | `TAnd (a, b), true ->
    `TOr (propagate_nots_aux ~neg a, propagate_nots_aux ~neg b)
  | `TOr (a, b), false ->
    `TOr (propagate_nots_aux ~neg a, propagate_nots_aux ~neg b)
  | `TOr (a, b), true ->
    `TAnd (propagate_nots_aux ~neg a, propagate_nots_aux ~neg b)
  | `TNot a, neg ->
    propagate_nots_aux ~neg:(not neg) a
  | `TAtom a, false ->
    `TAtomPos a
  | `TAtom a, true ->
    `TAtomNeg a
  | `TTrue, false -> `TTrue
  | `TTrue, true -> `TFalse
  | `TFalse, false -> `TFalse
  | `TFalse, true -> `TTrue

let propagate_nots (formula : formula) : formula2 =
  let res = propagate_nots_aux false formula in
  Options.debug ~level:debug_level "remove negations: @[%a@] -> @[%a@]"
    pp_formula formula pp_formula res;
  res

let rec to_dnf_aux n t =
  match t with
  | `TAnd (a, b) ->
    let a = to_dnf_aux n a in
    let b = to_dnf_aux n b in
    let res = dnf_and n a b in
    Options.debug ~level:(debug_level+1) "@[%a@] . @[%a@] = @[%a@]"
      Bes.pp_dnf_expression a Bes.pp_dnf_expression b Bes.pp_dnf_expression res;
    res
  | `TOr (a, b) ->
    let a = to_dnf_aux n a in
    let b = to_dnf_aux n b in
    let res = dnf_or n a b in
    Options.debug ~level:(debug_level+1) "@[%a@] + @[%a@] = @[%a@]"
      Bes.pp_dnf_expression a Bes.pp_dnf_expression b Bes.pp_dnf_expression res;
    res
  | `TAtomPos id -> dnf_var n id `True
  | `TAtomNeg id -> dnf_var n id `False
  | `TTrue -> dnf_true n
  | `TFalse -> dnf_false n

let to_dnf n (t : formula) =
  let res = to_dnf_aux n (propagate_nots t) in
  Options.debug ~level:debug_level "convert to dnf: @[%a@] -> @[%a@]"
    pp_formula t Bes.pp_dnf_expression res;
  res

let convert_back_minterm minterm : formula =
  let f ((i,acc) : int * formula list) atom =
    let acc = match atom with
      | `Dontcare -> acc
      | `True -> (`TAtom i) :: acc
      | `False -> (`TNot (`TAtom i)) :: acc
    in
    (i+1, acc)
  in
  let _, rev = List.fold_left f (0, []) minterm in
  match rev with
  | [] -> `TTrue
  | last :: rest ->
    List.fold_left (fun acc e -> `TAnd (e, acc)) last rest;;

let convert_back_dnf dnf : formula =
  match List.rev dnf with
  | [] -> `TFalse
  | head :: tail ->
    let head' = convert_back_minterm head in
    let f (acc : formula) e : formula =
      `TOr (convert_back_minterm e, acc)
    in
    List.fold_left f head' tail

let simplify n formula =
  let dnf = to_dnf n formula in
  let optdnf, _exact =  Bes.auto_optimize dnf in
  convert_back_dnf optdnf

module type BOOLEAN_CONVERTIBLE = sig
  type t
  type info
  val convert : ?info:info -> t -> int*info*formula
  val convert_back : info:info -> formula -> t
end


module Make (C : BOOLEAN_CONVERTIBLE) = struct

  let simplify e =
    let n, info, t1 = C.convert e in
    let optt1 = simplify n t1 in
    C.convert_back info optt1

end

module Exp = Make (struct

    type t = Cil_types.exp
    module ExpH = Cil_datatype.ExpStructEq.Hashtbl
    type info = int ExpH.t * t array
    open Ast_const

    type atom_map = int ExpH.t;;

    let rec fstpass (h : atom_map) n l e : int * exp list * formula =
      match e.enode with
      | BinOp (LAnd, a, b, _) ->
        let (na, la, a') = fstpass h n l a in
        let (nb, lb, b') = fstpass h na la b in
        (nb, lb, `TAnd (a', b'))
      | BinOp (LOr, a, b, _) ->
        let (na, la, a') = fstpass h n l a in
        let (nb, lb, b') = fstpass h na la b in
        (nb, lb, `TOr (a', b'))
      | UnOp (LNot, e, _) ->
        let (n', l', e') = fstpass h n l e in
        (n', l', `TNot e')
      | _ ->
        if ExpH.mem h e then
          (n, l, `TAtom (ExpH.find h e))
        else
          (ExpH.add h e n; (n+1, e :: l, `TAtom n))

    let convert ?info e =
      let h, n, l = match info with
        | Some (h, arr) -> (h, Array.length arr, List.rev (Array.to_list arr))
        | None -> ExpH.create 12, 0, []
      in
      let n, l, tmp = fstpass h n l e in
      (n, (h, Array.of_list (List.rev l)), tmp)

    let rec convert_back arr (phi:formula) : t =
      match phi with
      | `TAnd (a, b) ->
        Exp.binop LAnd (convert_back arr a) (convert_back arr b)
      | `TOr (a, b) ->
        Exp.binop LOr (convert_back arr a) (convert_back arr b)
      | `TNot a ->
        Exp.lnot (convert_back arr a)
      | `TAtom i ->
        arr.(i)
      | `TTrue ->
        Exp.one ()
      | `TFalse ->
        Exp.zero ()

    let convert_back ~info:(_, arr) phi =
      convert_back arr phi
  end)

let simplify_exp = Exp.simplify
