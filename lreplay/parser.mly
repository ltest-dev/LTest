/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2013-2018                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  You may redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 3.                                                */
/*                                                                        */
/*  It is distributed in the hope that it will be useful, but WITHOUT     */
/*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    */
/*  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General      */
/*  Public License for more details.                                      */
/*                                                                        */
/*  See the GNU Lesser General Public License version 3 for more          */
/*  details (enclosed in the file LICENSE).                               */
/*                                                                        */
/**************************************************************************/

%{
open Hhelpers
let parse_error s = raise (SyntaxError ("Error while parsing hyperlabel file: syntax error"))

%}

%token <int> LAB SEQ ID
%token <string> COND ENV VAR
%token PLUS 
%token DOT
%token LANGLE 
%token PIPE 
%token COMMA 
%token RANGLE 
%token LB 
%token RB 
%token EOF 
%token DASH 
%token COV 
%token UNCOV 
%token LSB 
%token RSB 
%token EQ 
%token NEQ 
%token AND 
%token SC

%left PLUS 
%left DOT

%start hyplist_root
%type <Hhelpers.node list> hyplist_root hyplist
%type <Hhelpers.node> hyp
%type <string> status var
%type <int> id
%type <(string * string * string) list> cond boolEq 
%%

hyplist_root:
  | EOF       { [] }
  | hyplist EOF { $1 }
  ;

hyplist:
    | hyplist id hyp status COMMA { (List.append $1 [(Hhelpers.annotateHLabel $2 $3 $4)]) }
    | id hyp status COMMA { [(Hhelpers.annotateHLabel $1 $2 $3)] }
    | COMMA { [] } 
    ; 

id : 
    | { -1 }
    | ID RB { $1 }
    ;

status : 
    | { "" }
    | DASH COV DASH ENV { "c:" ^ $4 }
    | DASH UNCOV DASH ENV { "u:" ^ $4 }
    ;

hyp:	
  | LAB  { Hhelpers.createDnfFromLabel $1 }
  | SEQ  { Hhelpers.createDnfFromSequence $1 } 
  | LANGLE hyp PIPE SC cond SC RANGLE  { Hhelpers.createDnfFromGuard $2 $5 }       
  | hyp DOT hyp { Hhelpers.createDnfFromConjunction $1 $3 } 
  | LB hyp DOT hyp RB { Hhelpers.createDnfFromConjunction $2 $4 } 
  | hyp PLUS hyp { Hhelpers.createDnfFromDisjunction $1 $3 }  
  | LB hyp PLUS hyp RB { Hhelpers.createDnfFromDisjunction $2 $4 }
  ;

cond: 
    | { [] }
    |  { [] }
    | boolEq { $1 }
    ;

boolEq: 
    | var EQ var  { [($1,"=",$3)] }
    | var NEQ var  { [($1,"!",$3)] }
    | boolEq AND var EQ var { $1 @ [($3,"=",$5)] }
    | boolEq AND var NEQ var { $1 @ [($3,"!",$5)] }
    ;

var:
    | VAR  { $1 }
    | LAB  { "l" ^ string_of_int $1 }
    | SEQ  { "s" ^ string_of_int $1 }
    ;
