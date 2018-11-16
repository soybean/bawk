type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | 
          Geq | And | Or | Pluseq | Minuseq | Strcat | Rgxeq | Rgxneq | 
          Rgxcomp | Rgxnot

type uop = Not | Neg | Access | Increment | Decrement

type typ = Int | Bool | Void | String | Rgx | ArrayType of typ

type bind = typ * string 

type expr = 
    Binop of expr * op * expr
  | BoolLit of bool
  | Literal of int
  | StringLiteral of string
  | RgxLiteral of string
  | Id of string
  | Assign of expr * expr
  | Call of string * expr list
  | Rgx of string
  | Unop of uop * expr
  | ArrayLit of expr list
  | ArrayDeref of expr * expr
  | NumFields
  | Noexpr

type config_expr = 
    RSAssign of expr
  | FSAssign of expr

type stmt = 
    Return of expr
  | Expr of expr
  | Block of stmt list
  | While of expr * stmt
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | EnhancedFor of string * stmt

type func_decl = {
    ret_type : typ;
    fname    : string;
    formals  : bind list;
    locals   : bind list;
    body     : stmt list;
  }

type begin_list = bind list * func_decl list
type loop_list = bind list * stmt list
type end_list = bind list * stmt list
type config_list = config_expr list

type program = begin_list * loop_list * end_list * config_list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "==" 
  | Neq -> "!="
  | Less -> "<" 
  | Leq -> "<=" 
  | Greater -> ">" 
  | Geq -> ">=" 
  | And -> "&&" 
  | Or -> "||" 
  | Pluseq -> "+=" 
  | Minuseq -> "-=" 
  | Strcat -> "&" 
  | Rgxeq -> "%" 
  | Rgxneq -> "!%"
  | Rgxcomp -> "~" 
  | Rgxnot -> "!~"


let string_of_uop = function
    Neg -> "-"
  | Not -> "!"
  | Access -> "$"
  | Increment -> "++"
  | Decrement -> "--"

let rec string_of_expr = function
    Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Literal(l) -> string_of_int l
  | StringLiteral(s) -> s
  | RgxLiteral(r) -> r
  | Id(s) -> s
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e  
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Rgx(r) -> r
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e  
  | ArrayLit(el) -> "[" ^ String.concat ", " (List.map string_of_expr el) ^ "]"
  | ArrayDeref(v, e) -> string_of_expr v ^ "[" ^ string_of_expr e ^ "]"
  | NumFields -> ""
  | Noexpr -> ""

let string_of_config_expr = function
    RSAssign(e) -> "RS = " ^ string_of_expr e ^ "\n"
  | FSAssign(e) -> "FS = " ^ string_of_expr e ^ "\n"

let rec string_of_stmt = function
    Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | EnhancedFor(str, s) -> "for (" ^ str ^ " in " ^ string_of_stmt s ^ ")"

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
  | String -> "string"
  | Rgx -> "rgx"
  | ArrayType(t) -> string_of_typ t ^ "[]"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  "function " ^ string_of_typ fdecl.ret_type ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_beginBlock (globals, funcs) = 
  "BEGIN {\n" ^ String.concat "" (List.map string_of_vdecl globals) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^ "\n}\n"

let string_of_loopBlock (locals, stmts) = 
  "LOOP {\n" ^ String.concat "" (List.map string_of_vdecl locals) ^ "\n" ^
  String.concat "\n" (List.map string_of_stmt stmts) ^ "\n}\n"

let string_of_endBlock (locals, stmts) = 
  "END {\n" ^ String.concat "" (List.map string_of_vdecl locals) ^ "\n" ^
  String.concat "\n" (List.map string_of_stmt stmts) ^ "\n}\n"

let string_of_configBlock (configs) = 
  "CONFIG {\n" ^ String.concat "" (List.map string_of_config_expr configs) ^ "\n}\n"

let string_of_program(beginBlock, loopBlock, endBlock, configBlock) =
  "" ^ string_of_beginBlock beginBlock ^ "\n" ^
  "" ^ string_of_loopBlock loopBlock ^ "\n" ^
  "" ^ string_of_endBlock endBlock ^ "\n" ^
  "" ^ string_of_configBlock configBlock

