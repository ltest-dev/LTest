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

let template_regexp = Str.regexp "\\$\\(\\{[^\\}]*\\}\\|\\$\\)"

let apply_template f template =
  let g s =
    let m = Str.matched_string s in
    if m = "$$" then "$"
    else f (String.sub m 2 ((String.length m)-3))
  in
  Str.global_substitute template_regexp g template

let warning_fallback var =
  Format.eprintf "[warning] unknown variable (%s)\n" var;
  ""

let apply_template_hashtbl ?(fallback=warning_fallback) table template =
  let f var =
    try
      Hashtbl.find table var
    with Not_found ->
      fallback var
  in
  apply_template f template

(*****************************************************************************)

let fnmatch_special_regexp = Str.regexp "\\*\\|\\?\\|\\[[!^]?\\]?[^]]*\\]"

let rec has_delim l =
  match l with
  | [] -> false
  | Str.Delim _ :: _ -> true
  | Str.Text _ :: tail -> has_delim tail

type fnmatch = FnString of string | FnRegexp of string*Str.regexp

let fnmatch_compile pattern =
  let f m =
    match m.[0] with
    | '?' -> ".?"
    | '*' -> ".*"
    | '[' ->
      let start, prefix = match m.[1] with
      | '^' | '!' -> (2, "[^")
      | _ -> (1, "[")
      in
      let len = (String.length m) - 1 - start in
      prefix^(Str.quote (String.sub m start len))^"]"
    | _ -> assert false
  in
  let g item =
    match item with
    | Str.Text t -> Str.quote t
    | Str.Delim t -> f t
  in
  let split = Str.full_split fnmatch_special_regexp pattern in
  if has_delim split then
    let regexp = "^"^(String.concat "" (List.map g split))^"$" in
    FnRegexp (pattern, Str.regexp regexp)
  else
    FnString pattern

let fnmatch_match compiled str =
  match compiled with
  | FnString str' -> str = str'
  | FnRegexp (_,regexp) -> Str.string_match regexp str 0

let fnmatch pattern str =
  fnmatch_match (fnmatch_compile pattern) str

(*****************************************************************************)

let glob_special_regexp = Str.regexp "/\\|\\[\\]?[^]]*\\]"

let glob_prepare_add_head t acc =
  match acc with
  | [] -> [t]
  | head :: tail -> (t^head)::tail

let glob_prepare_split_result acc result =
  match result with
  | Str.Text t -> t :: acc
  | Str.Delim t ->
    match t.[0] with
    | '[' -> glob_prepare_add_head t acc
    | '/' -> (* ensures that we have something if it's starting with / *)
      glob_prepare_add_head "" acc
    | _ -> assert false

let glob_prepare pattern =
  let l = List.fold_left glob_prepare_split_result [] (Str.full_split glob_special_regexp pattern) in
  List.rev_map fnmatch_compile l

let rec glob_aux ~sort f base rest =
  match rest with
  | [] ->
    f base
  | head :: rest ->
    if base = "" || Sys.is_directory base then
      match head with
      | FnString str ->
        if str = "" then glob_aux ~sort f base rest
        else
          let newbase = Filename.concat base str in
          if Sys.file_exists newbase then glob_aux ~sort f newbase rest
      | FnRegexp (pattern, regexp) ->
        let g str =
          if Str.string_match regexp str 0 then
            glob_aux ~sort f (Filename.concat base str) rest
        in
        let content = Sys.readdir base  in
        (if sort then Array.sort compare content);
        Array.iter g content

let glob ?(sort=false) f pattern =
  match glob_prepare pattern with
  | FnString nil :: rest when nil =  "" ->
    glob_aux ~sort f "/" rest
  | rest ->
    glob_aux ~sort f "" rest

let glob_list ?sort pattern =
  let res = ref [] in
  let f file = res := file :: !res in
  glob ?sort f pattern;
  List.rev !res

(*****************************************************************************)

let apos_regexp = Str.regexp "'"

let shell_escape str =
  let str = Str.global_replace apos_regexp "'\''" str in
  "'"^str^"'"

(*****************************************************************************)

let explode s sep =
  let len = String.length s
  and last = ref 0
  and acc = ref [] in
  let f i c =
    if String.contains sep c then
      let start = !last in
      if start < i then
        acc := String.sub s start (i-start) :: !acc;
      last := i+1
  in
  String.iteri f s;
  let start = !last in
  if start < len then
    acc := String.sub s start (len-start) :: !acc;
  List.rev !acc


let get_file_from_path path =
  let splitted = String.split_on_char '/' path in
  match  splitted with
  | [] -> ""
  | [s] -> s
  | _ -> List.hd (List.rev splitted)


let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false
