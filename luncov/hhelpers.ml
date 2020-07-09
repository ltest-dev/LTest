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

type status = Unknown of string
            | Uncoverable of string
            | Covered of string

type node_elt = {
  labels: int list;
  sequences : int list;
  condition : (string * string * string) list
}

type node = {
  mutable id: int;
  ls: node_elt list;
  mutable status: status
}

let annotateHLabel i e s = {id=i; ls=e.ls; status=s; }

let is_unknown hl =
  match hl.status with
  | Unknown _ -> true
  | _ -> false

let string_of_intlist st il =
  let l = List.fold_right (fun i s -> st ^ (string_of_int i) ^ "." ^ s) il "" in
  String.sub l 0 (max 0 ((String.length l) - 1))

let string_of_tuple (va,t,vb) =
  match va,t,vb with
  | (va,"=",vb) -> va ^ "==" ^ vb
  | (va,"!",vb) -> va ^ "!=" ^ vb
  | _ -> raise (Invalid_argument "")

let combine_cond l r = if (String.length r = 0) then l else (l ^ " && " ^ r)

let string_of_cond c = if c = [] then "" else List.fold_right combine_cond (List.map string_of_tuple c) ""

let string_of_env env =
  if env = "" then "[ ]" else env

let string_of_status status =
  match status with
  | Unknown env -> " - UNKNOWN - " ^ (string_of_env env)
  | Uncoverable env -> " - UNCOVERABLE - " ^ (string_of_env env)
  | Covered env -> " - COVERED - " ^ (string_of_env env)

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
  ^ (string_of_status n.status)
  ^ ","

exception SyntaxError of string

let combine elle elri = {labels = elle.labels @ elri.labels;
                         sequences = elle.sequences @ elri.sequences;
                         condition = elle.condition @ elri.condition}

let createDnfFromLabel l = {id = -1;
                            ls = [{labels = [l]; sequences = []; condition = []}];
                            status = Unknown ""}

let createDnfFromSequence s = {id = -1;
                               ls = [{labels = []; sequences = [s]; condition = []}];
                               status = Unknown ""}

let createDnfFromGuard h c = {id = -1;
                              ls = List.map (fun el -> {
                                    labels = el.labels;
                                    sequences = el.sequences;
                                    condition = el.condition @ c
                                  }) h.ls;
                              status = Unknown ""}

let createDnfFromConjunction le ri =
  {id = -1;
   ls = List.concat (List.map (fun elle ->
       List.map (fun elri -> combine elle elri) ri.ls
     ) le.ls);
   status = Unknown ""}

let createDnfFromDisjunction le ri = {id = -1;
                                      ls = le.ls @ ri.ls;
                                      status = Unknown ""}
