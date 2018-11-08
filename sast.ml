open Ast

type sbind = typ * string 
type sexpr = typ * sx and
sx = SBinop of sexpr * op * sexpr
| SBoolLit of bool
| SLiteral of int
| SId of string
| SAssign of string * sexpr
| SCall of string * sexpr list
| SRgx of string
| SUnop of uop * sexpr
| SInitIntArrLit of string * sexpr list
| SInitStrArrLit of string * sexpr list
| SInitBoolArrLit of string * sexpr list
| SInitRgxArrLit of string * sexpr list
| SInitMapLit of typ * typ * string * sexpr list
| SInitEmptyMap of typ * typ * string
| SAssignElement of string * sexpr * sexpr
| SGetElement of string * sexpr
| SNumFields

type sconfig_expr = SRSAssign of sexpr
| SFSAssign of sexpr

type sstmt = SReturn of sexpr
| SExpr of sexpr
| SBlock of Sstmt list
| SWhile of sexpr * sstmt
| SIf of expr * sstmt * sstmt
| SInitEmptyMap of typ * typ * string
| SFor of sexpr * sexpr * sexpr * sstmt
| SEnhancedFor of typ * string * sstmt
| SAssignElement of string * sexpr * sexpr
| SGetElement of string * sexpr

type sfunc_decl = {
  sret_type : typ;
  sfname    : string;
  sformals  : sbind list;
  slocals   : sbind list;
  sbody     : sstmt list;
}


type sbegin_list = sbind list * sfunc_decl list

type sloop_list = sbind list * sstmt list

type send_list = sbind list * sstmt list

type sconfig_list = sconfig_expr list

type sprogram = sbegin_list * sloop_list * send_list * sconfig_list
