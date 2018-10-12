%{ open Ast %}

%token LPAREN RPAREN LCURLY RCURLY SEMI COMMA
%token PLUS MINUS DIVIDE MULTIPLY ASSIGN
%token EQUALS NEQ LT LEQ GT GEQ AND OR NOT
%token PLUSEQ MINUSEQ INCREMENT DECREMENT

%token RETURN FUNCTION 
%token CONFIG BEGIN LOOP END
%token INT BOOL VOID STRING RGX TRUE FALSE
%token RS FS
%token STRCAT
%token RGXEQ RGXNEQ RGXSTRCMP RGXSTRNOT

%token <int> LITERAL
%token <string> ID
%token EOF

%right ASSIGN PLUSEQ MINUSEQ
%left OR
%left AND
%left EQUALS NEQ RGXEQ RGXNEQ RGXSTRCMP RGXSTRNOT

%left LT LEQ GT GEQ
%left PLUS MINUS STRCAT
%left MULTIPLY DIVIDE
%right NOT INCREMENT DECREMENT

%start program
%type <Ast.program> program

%%
program: config_block begin_block loop_block end_block EOF { ($1, $2, $3, $4) }

begin_block: BEGIN LCURLY func_list global_vars_list RCURLY 
{ [func_list, global_vars_list] }

loop_block: LOOP LCURLY stmt_list local_vars_list RCURLY 
{ [ stmt_list, local_vars_list] }

end_block: END LCURLY stmt_list local_vars_list RCURLY 
{ [stmt_list, local_vars_list] }

config_block:							{ [] }
| CONFIG LCURLY config_expr_list RCURLY	{ [config_expr_list] }

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

config_expr_list: 				{ [] }
| config_expr_list config_expr	{ $2 :: $1}

config_expr: RS ASSIGN expr { RSAssign($3) }
| FS ASSIGN expr 			{ FSAssign($3) }

stmt_list: 		{ [] } 
| stmt_list stmt 	{ $2 :: $1 } 

stmt: expr SEMI 		{ Expr $1 } 
| RETURN expr SEMI 		{ Return $2 } 
| LCURLY stmt_list RCURLY 	{ Block(List.rev $2) }

expr: LITERAL { Literal($1) } 
| TRUE { BoolLit(true) } 
| FALSE { BoolLit(false) } 
| ID { Id($1) } 
| expr PLUS expr { Binop($1, Add, $3) } 
| expr MINUS expr { Binop($1, Sub, $3) } 
| expr MULTIPLY expr { Binop($1, Mult, $3) } 
| expr DIVIDE expr { Binop($1, Div, $3) } 
| expr EQUALS expr { Binop($1, Equal, $3) } 
| expr NEQ expr { Binop($1, Neq, $3) }
| expr LT expr { Binop($1, Less, $3) } 
| expr LEQ expr { Binop($1, Leq, $3) } 
| expr GT expr { Binop($1, Greater, $3) } 
| expr GEQ expr { Binop($1, Geq, $3) } 
| expr AND expr { Binop($1, And, $3) } 
| expr OR expr { Binop($1, Or, $3) }
| expr PLUSEQ expr { Binop($1, Pluseq, $3) } 
| expr MINUSEQ expr { Binop($1, Minuseq, $3) }
| expr INCREMENT { Binop($1, Add, 1) }
| expr DECREMENT { Binop($1, Sub, 1) }
| expr STRCAT expr { Binop($1, Strcat, $3) }
| expr RGXEQ expr { Binop ($1, Rgxeq, $3) }
| expr RGXNEQ expr { Binop ($1, Rgxneq, $3)}
| expr RGXSTRCMP expr { Binop ($1, Rgxcomp, $3)}
| expr RGXSTRNOT expr { Binop ($1, Rgxnot, $3)}
| NOT expr { Unop(Not, $2) }
| ID ASSIGN expr { Assign($1, $3) } 
| LPAREN expr RPAREN { $2 } 
| ID LPAREN actuals_opt RPAREN { Call($1, $3) }

actuals_opt: { [] } 
| actuals_list { List.rev $1 } 

actuals_list: expr { [$1] } 
| actuals_list COMMA expr { $3 :: $1 }
