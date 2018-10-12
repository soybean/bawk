%{ open Ast %}

%token LPAREN RPAREN LCURLY RCURLY SEMI COMMA
%token PLUS MINUS DIVIDE MULTIPLY ASSIGN
%token EQUALS
%token RETURN FUNCTION 
%token BEGIN
%token INT BOOL VOID STRING RGX TRUE FALSE

%token <int> LITERAL
%token <string> ID
%token EOF

%right ASSIGN
%left EQUALS
%left PLUS MINUS
%left MULTIPLY DIVIDE

%start program
%type <Ast.program> program

%%

(* DECLARATIONS *)

program: begin_block EOF { $1 }

begin_block: BEGIN LCURLY func_list global_vars_list RCURLY 
{ [func_list, global_vars_list] }

typ: STRING	{ String }
| INT 		{ Int }
| BOOL		{ Bool }
| VOID          { Void }
| RGX           { Rgx }

func_list: 		{ [] }
| func_list func	{ $2 :: $1 }

global_vars_list: 		{ [] }
| global_vars_list var_decl	{ $2 :: $1 }

local_vars_list:
				{ [] }
| local_vars_list var_decl	{ $2 :: $1 }

func: FUNCTION ID LPAREN formals_opt RPAREN typ LCURLY local_vars_list stmt_list RCURLY 
{ { fname = $2; formals = $4; ret_type = $6; 
	locals = $8;  body = List.rev $9 } } 

formals_opt: 	{ [] }
| formals_list	{ List.rev $1 }

formals_list: typ ID		{ [($1, $2)] }
| formals_list COMMA typ ID 	{ ($3, $4) :: $1 }

var_decl: typ ID SEMI { ($1, $2) }

(* STATEMENTS *)

stmt_list: 		{ [] } 
| stmt_list stmt 	{ $2 :: $1 } 

stmt: expr SEMI 		{ Expr $1 } 
| RETURN expr SEMI 		{ Return $2 } 
| LCURLY stmt_list RCURLY 	{ Block(List.rev $2) }

(* EXPRESSIONS *)

expr: LITERAL { Literal($1) } 
| TRUE { BoolLit(true) } 
| FALSE { BoolLit(false) } 
| ID { Id($1) } 
| expr PLUS expr { Binop($1, Add, $3) } 
| expr MINUS expr { Binop($1, Sub, $3) } 
| expr MULTIPLY expr { Binop($1, Mult, $3) } 
| expr DIVIDE expr { Binop($1, Div, $3) } 
| expr EQUALS expr { Binop($1, Equal, $3) } 
| ID ASSIGN expr { Assign($1, $3) } 
| LPAREN expr RPAREN { $2 } 
| ID LPAREN actuals_opt RPAREN { Call($1, $3) }

actuals_opt: { [] } 
| actuals_list { List.rev $1 } 

actuals_list: expr { [$1] } 
| actuals_list COMMA expr { $3 :: $1 }
