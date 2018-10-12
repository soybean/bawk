type op = Add | Sub | Mult | Div | Assn
type typ = Int | Bool | String 
type bind = typ * string 
type expr = Binop of expr * op * expr
| BoolLit of bool
| Literal of int
| Id of string
| Assign of string * expr
| Call of string * expr list

type stmt = 
| Return of expr
| Expr of expr
| Block of stmt list

type func_decl = {
  ret_type : typ;
  fname    : string;
  formals  : bind list;
  locals   : bind list;
  body     : stmt list;
}

type program = bind list * func_decl list
