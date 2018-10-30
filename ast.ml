type op = Add | Sub | Mult | Div | Assn | Equal | Neq | Less | Leq | Greater | 
		  Geq | And | Or | Pluseq | Minuseq | Strcat | Rgxeq | Rgxneq | 
		  Rgxcomp | Rgxnot
type uop = Not | Access | InitIntArr | InitStrArr | InitBoolArr | InitRgxArr | Neg | AssignElement | GetElement
type typ = Int | Bool | Void | String | Rgx
type bind = typ * string 
type expr = Binop of expr * op * expr
| BoolLit of bool
| Literal of int
| Id of string
| Assign of string * expr
| Call of string * expr list
| Rgx of string
| Unop of uop * expr
| InitIntArrLit of string * expr list
| InitMapLit of typ * typ * string * expr list
| AssignElement of string * expr * expr
| GetElement of string * expr
| InitBoolArrLit of string * expr list
| InitStringArrLit of string * expr list 

type config_expr = RSAssign of config_expr * expr
| FSAssign of config_expr * expr


type stmt = Return of expr
| Expr of expr
| Block of stmt list
| While of expr * stmt
| If of expr * stmt * stmt
| InitEmptyMap of typ * typ * string
| For of expr * expr * expr * stmt
| EnhancedFor of typ * string * stmt
| AssignElement of string * expr * expr
| GetElement of string * expr

type func_decl = {
  ret_type : typ;
  fname    : string;
  formals  : bind list;
  locals   : bind list;
  body     : stmt list;
}

(*type begin_list =*) 
type begin_list = bind list * func_decl list

type program = config_expr list * (bind list * func_decl list) list * (bind list * stmt list) list * (bind list * stmt list) list
(*type program = func_decl list * bind list * func_decl list * bind list * func_decl list * bind list * func_decl list * bind list*)
(*type program = bind list * func_decl list*)


let string_of_program(config, beginBlock, loop, endBlock) = "HI"
