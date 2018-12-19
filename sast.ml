(* Authors: Ashley, Christine, Melanie, Victoria *)

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
  | SArrayLit of sexpr list
  | SArrayDeref of sexpr * sexpr
  | SAccess of sexpr
  | SPluseq of sexpr * sexpr
  | SMinuseq of sexpr * sexpr
  | SIncrement of sexpr
  | SDecrement of sexpr
  | SStrcat of sexpr * sexpr
  | SRgxeq of sexpr * sexpr
  | SRgxneq of sexpr * sexpr
  | SRgxcomp of sexpr * sexpr
  | SRgxnot of sexpr * sexpr
  | SNumFields
  | SNoexpr

type sstmt = 
    SReturn of sexpr
  | SExpr of sexpr
  | SBlock of sstmt list
  | SWhile of sexpr * sstmt
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SEnhancedFor of string * string * sstmt

type sfunc_decl = {
    sret_type : typ;
    sfname    : string;
    sformals  : bind list;
    slocals   : bind list;
    sbody     : sstmt list;
  }

type sbegin_list = bind list * sfunc_decl list
type sloop_list = sfunc_decl
type send_list = sfunc_decl
type sconfig_pair = string * string (* RS * FS *)

type sprogram = sbegin_list * sloop_list * send_list * sconfig_pair

(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) = 
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SLiteral(l) -> string_of_int l
  | SStringLiteral(s) -> s
  | SRgxLiteral(r) -> r
  | SId(s) -> s
  | SAssign(v, e) -> string_of_sexpr v ^ " = " ^ string_of_sexpr e  
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SRgx(r) -> r
  | SStrcat(a, b) -> string_of_sexpr a ^ string_of_sexpr b
  | SRgxcomp(a, b) -> string_of_sexpr a ^ string_of_sexpr b
  | SRgxnot(a, b) -> string_of_sexpr a ^ string_of_sexpr b
  | SRgxeq(a, b) -> string_of_sexpr a ^ string_of_sexpr b
  | SRgxneq(a, b) -> string_of_sexpr a ^ string_of_sexpr b
  | SPluseq(a, b) -> string_of_sexpr a ^ string_of_sexpr b
  | SMinuseq(a, b) -> string_of_sexpr a ^ string_of_sexpr b
  | SIncrement(a) -> string_of_sexpr a
  | SDecrement(a) -> string_of_sexpr a
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e  
  | SArrayLit(el) -> "[" ^ String.concat ", " (List.map string_of_sexpr el) ^ "]"
  | SArrayDeref(v, e) -> string_of_sexpr v ^ "[" ^ string_of_sexpr e ^ "]"
  | SAccess(e) -> "$" ^ string_of_sexpr e
  | SNumFields -> "NF"
  | SNoexpr -> ""
          ) ^ ")"

let string_of_config_pair (rs, fs) = 
  "RS = " ^ rs ^ "\nFS = " ^ fs ^ "\n"

let rec string_of_sstmt = function
    SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SIf(e, s, SBlock([])) -> "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SEnhancedFor(str1, str2, s) -> "for (" ^ str1 ^ " in " ^ str2 ^ ") " ^ string_of_sstmt s

let string_of_sfdecl fdecl =
  "function " ^ string_of_typ fdecl.sret_type ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sbeginBlock (globals, funcs) = 
  "BEGIN {\n" ^ String.concat "" (List.map string_of_vdecl globals) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs) ^ "\n}\n"

let string_of_sloopBlock (loop_func) = 
  "LOOP {\n" ^ String.concat "" (List.map string_of_vdecl loop_func.slocals) ^ "\n" ^
  String.concat "\n" (List.map string_of_sstmt loop_func.sbody) ^ "\n}\n"

let string_of_sendBlock (end_func) = 
  "END {\n" ^ String.concat "" (List.map string_of_vdecl end_func.slocals) ^ "\n" ^
  String.concat "\n" (List.map string_of_sstmt end_func.sbody) ^ "\n}\n"

let string_of_sconfigBlock (configs) = 
  "CONFIG {\n" ^ (string_of_config_pair configs) ^ "\n}\n"

let string_of_sprogram(beginBlock, loopBlock, endBlock, configBlock) =
  "" ^ string_of_sbeginBlock beginBlock ^ "\n" ^
  "" ^ string_of_sloopBlock loopBlock ^ "\n" ^
  "" ^ string_of_sendBlock endBlock ^ "\n" ^
  "" ^ string_of_sconfigBlock configBlock

