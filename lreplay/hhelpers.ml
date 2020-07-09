(**************************************************************************)
(*                                                                        *)
(*  This file is part of LReplay.                                         *)
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

type node_elt = {
  labels: int list;
  sequences : int list;
  condition : (string * string * string) list
}

type node = {
  mutable id: int;
  ls: node_elt list;
  mutable status: string
}

let annotateHLabel i e s = {id=i; ls=e.ls; status=s; }

let string_of_intlist st il =
  let l = List.fold_right (fun i s -> st ^ (string_of_int i) ^ "." ^ s) il "" in
  String.sub l 0 (max 0 ((String.length l) - 1))

let string_of_tuple (va,t,vb) =
  match va,t,vb with
  | (va,"=",vb) -> va ^ "==" ^ vb
  | (va,"!",vb) -> va ^ "!=" ^ vb
  | (va,t,b) -> raise (Invalid_argument "")

let combine_cond l r = if (String.length r = 0) then l else (l ^ " && " ^ r)

let string_of_cond c = if c = [] then " " else List.fold_right combine_cond (List.map string_of_tuple c) ""

let string_of_node n =
  (if n.id >=0 then (string_of_int n.id) ^ ") " else "") ^
  (let st =
     (List.fold_right (fun nel s ->
          "<"
          ^ (string_of_intlist "l" nel.labels)
          ^ (if (((List.length nel.labels) > 0) && ((List.length nel.sequences) > 0)) then "." else "")
          ^ (string_of_intlist "s" nel.sequences)
          ^ "|;"
          ^ (string_of_cond nel.condition)
          ^ ";> + "
          ^ s) n.ls "")
   in
   (String.sub st 0 ((String.length st) - 3)))
  ^ n.status
  ^ ","

exception SyntaxError of string

(*let check_label_exists l = ()

let check_sequence_exists l = ()

let get_labels n = List.fold_right (fun el l -> (el.labels @ l)) n []

let get_sequences n = List.fold_right (fun el l -> (el.sequences @ l)) n []

let check_sharing_labels la lb = List.find (fun el -> List.mem el (get_labels la)) (get_labels lb)

let check_sharing_sequences la lb = List.find (fun el -> List.mem el (get_sequences la)) (get_sequences lb)

let check_well_formed_conj le ri = try
  let duplicate = check_sharing_labels le ri
  in raise (SyntaxError ("[hreplay] Error while parsing hyperlabel file: label " ^ (string_of_int duplicate)  ^ " appears on two side of a ."))
  with Not_found -> begin
  try
  let duplicate = check_sharing_sequences le ri
  in raise (SyntaxError ("[hreplay] Error while parsing hyperlabel file: sequence " ^ (string_of_int duplicate)  ^ " appears on two side of a ."))
  with Not_found -> ()
  end

let check_well_formed_disj le ri =
  let have_same_labels = (List.for_all (fun el -> List.mem el (get_labels ri)) (get_labels le)) and (List.for_all (fun el -> List.mem el (get_labels le)) (get_labels ri))
  in
  let have_same_sequences = (List.for_all (fun el -> List.mem el (get_sequences ri)) (get_sequences le)) and (List.for_all (fun el -> List.mem el (get_sequences le)) (get_sequences ri)) in if have_same_labels and have_same_sequences then () else raise (SyntaxError ("[hreplay] Error while parsing hyperlabel file: different  appears on two side of a +"))*)


let combine elle elri = {labels = elle.labels @ elri.labels; sequences = elle.sequences @ elri.sequences; condition=elle.condition @ elri.condition;}

let createDnfFromLabel l = (*check_label_exists l;*) {id = -1; ls = [{labels = [l]; sequences = []; condition = [];}]; status = "";}

let createDnfFromSequence s = (*check_sequence_exists s;*) {id = -1; ls = [{labels = []; sequences = [s]; condition = [];}]; status = "";}

let createDnfFromGuard h c = {id = -1; ls = (List.map (fun el -> {labels = el.labels; sequences = el.sequences; condition = el.condition @ c;}) h.ls); status = "";}

let createDnfFromConjunction le ri = (*(check_well_formed_conj le ri);*) {id = -1; ls = (List.concat (List.map (fun elle -> (List.map (fun elri -> combine elle elri) ri.ls)) le.ls)); status = "";}

let createDnfFromDisjunction le ri = (*(check_well_formed_disj le ri);*) {id = -1; ls = (le.ls @ ri.ls); status = "";}
