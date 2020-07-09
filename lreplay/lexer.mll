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

{
open Lexing
open Hhelpers
open Parser

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let env = '[' ( '(' | ')' | ',' | '-' | [' ' '\t'] | ['0'-'9'] | ['a'-'z'] | ['A'-'Z'] )* ']'
let id = ['1'-'9']['0'-'9']* | '0'
let lab = 'l'['0'-'9']+
let seq = 's'['0'-'9']+
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let var = (['a'-'z'] | ['A'-'Z']) (['a'-'z'] | ['A'-'Z'] | ['0'-'9'])*

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | '+'      { PLUS }
  | '.'      { DOT }
  | '<'      { LANGLE }
  | '|'      { PIPE }
  | ','      { COMMA }
  | '>'      { RANGLE }
  | '('      { LB }
  | ')'      { RB }
  | '-'      { DASH }
  | 'C''O''V''E''R''E''D'      { COV }
  | 'U''N''K''N''O''W''N'      { UNK }
  | 'U''N''C''O''V''E''R''A''B''L''E'  { UNCOV }
  | '=''='      { EQ }
  | '!''='      { NEQ }
  | '&''&'      { AND }
  | ';'      { SC }
  | lab      { let str = (Lexing.lexeme lexbuf) in LAB (int_of_string (String.sub str 1 ((String.length str) - 1))) }
  | seq      { let str = (Lexing.lexeme lexbuf) in SEQ (int_of_string (String.sub str 1 ((String.length str) - 1))) }
  | var      { VAR (Lexing.lexeme lexbuf) }
  | id      { ID (int_of_string (Lexing.lexeme lexbuf)) }
  | env      { ENV (Lexing.lexeme lexbuf) }
  | _ { raise (SyntaxError ("Error while parsing hyperlabel file: unexpected character '" ^ Lexing.lexeme lexbuf ^ "'")) }
  | eof      { EOF }

