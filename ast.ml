type op = Add | Sub | Mult | Div | Assn | Equal | Neq | Less | Leq | Greater | 
		  Geq | And | Or | Pluseq | Minuseq | Strcat | Rgxeq | Rgxneq | 
		  Rgxcomp | Rgxnot
type uop = Not | Access | Neg | AssignElement | GetElement | Increment | Decrement
type typ = Int | Bool | Void | String | Rgx
type bind = typ * string 
type expr = Binop of expr * op * expr
| BoolLit of bool
| Literal of int
| StringLiteral of string
| Id of string
| Assign of string * expr
| Call of string * expr list
| Rgx of string
| Unop of uop * expr
| InitIntArrLit of string * expr list
| InitStrArrLit of string * expr list
| InitBoolArrLit of string * expr list
| InitRgxArrLit of string * expr list
| InitMapLit of typ * typ * string * expr list
| InitEmptyMap of typ * typ * string
| AssignElement of string * expr * expr
| GetElement of string * expr
| NumFields

type config_expr = RSAssign of expr
| FSAssign of expr


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


type begin_list = bind list * func_decl list

type loop_list = bind list * stmt list

type end_list = bind list * stmt list

type config_list = config_expr list


type program = begin_list * loop_list * end_list * config_list

(* Pretty-printing functions *)

let string_of_program(beginBlock, loop, endBlock, configBlock) = "HI"
