open Ast

type sexpr = typ * sx
and sx =
| SBoolLit of bool
| SLiteral of int
| SStringLiteral of string
| SRgxLiteral of string
| SId of string
| SAssign of string * sexpr
| SCall of string * sexpr list
| SRgx of string
| SUnop of uop * sexpr
| SArrayLit of sexpr list
| SMapLit of typ * typ * string * sexpr * sexpr
| SInitEmptyMap of typ * typ * string
| SArrayAssignElement of string * sexpr * sexpr
| SArrayGetElement of string * sexpr
| SNumFields

type sconfig_expr = SRSAssign of sexpr
| SFSAssign of sexpr


type sstmt = SReturn of sexpr
| Sxpr of sexpr
| SBlock of sstmt list
| SWhile of sexpr * sstmt
| SIf of sexpr * sstmt * sstmt
| SInitEmptyMap of typ * typ * string
| SFor of sexpr * sexpr * expr * sstmt
| SEnhancedFor of string * sstmt
| SArrayAssignElement of string * sexpr * sexpr
| SArrayGetElement of string * sexpr

type sfunc_decl = {
  sret_type : typ;
  sfname    : string;
  sformals  : bind list;
  slocals   : bind list;
  sbody     : sstmt list;
}

type sbegin_list = bind list * func_decl list
type sloop_list = bind list * stmt list
type send_list = bind list * stmt list
type sconfig_list = config_expr list

type sprogram = sbegin_list * sloop_list * send_list * sconfig_list

(* Pretty-printing functions *)

let string_of_sprogram(beginBlock, loop, endBlock, configBlock) = "HI"