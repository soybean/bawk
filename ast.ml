type op = Add | Sub | Mult | Div | Assn | Equal | Neq | Less | Leq | Greater | 
		  Geq | And | Or | Pluseq | Minuseq | Strcat | Rgxeq | Rgxneq | 
		  Rgxcomp | Rgxnot
type uop = Not | Access | InitIntArr | InitStrArr | InitBoolArr | InitRgxArr | Neg
type typ = Int | Bool | Void | String | Rgx
type bind = typ * string 
type expr = Binop of expr * op * expr
| BoolLit of bool
| Literal of int
| Id of string
| Assign of string * expr
| Call of string * expr list
| Rgx of rgx
| Unop of uop * expr

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
| InitIntArrLit of string * list
| InitMapLit of typ * typ * string * list

type func_decl = {
  ret_type : typ;
  fname    : string;
  formals  : bind list;
  locals   : bind list;
  body     : stmt list;
}

type program = bind list * func_decl list
