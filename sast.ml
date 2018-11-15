open Ast

type sexpr = typ * sx
and sx = 
    SBinop of sexpr * op * sexpr
  | SBoolLit of bool
  | SLiteral of int
  | SStringLiteral of string
  | SRgxLiteral of string
  | SId of string
  | SAssign of sexpr * sexpr
  | SCall of string * sexpr list
  | SRgx of string
  | SUnop of uop * sexpr
  | ArrayLit of sexpr list
  | ArrayDeref of sexpr * sexpr
  | SNumFields
  | SNoexpr

type sconfig_expr = 
    SRSAssign of sexpr
  | SFSAssign of sexpr

type sstmt = 
    SReturn of sexpr
  | SExpr of sexpr
  | SBlock of sstmt list
  | SWhile of sexpr * sstmt
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SEnhancedFor of string * sstmt

type sfunc_decl = {
    sret_type : typ;
    sfname    : string;
    sformals  : bind list;
    slocals   : bind list;
    sbody     : sstmt list;
  }

type sbegin_list = bind list * sfunc_decl list
type sloop_list = bind list * sstmt list
type send_list = bind list * sstmt list
type sconfig_list = sconfig_expr list

type sprogram = sbegin_list * sloop_list * send_list * sconfig_list

(* Pretty-printing functions *)

let string_of_sprogram(beginBlock, loopBlock, endBlock, configBlock) = "PASS\n"
