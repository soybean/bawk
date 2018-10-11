%{ open Ast % }
%token LPAREN RPAREN LCURLY RCURLY LSQUARE RSQUARE COMMENT SEMI COMMA
%token PLUS MINUS DIVIDE ASSIGN STRCAT
%token EQUALS NEQ LT LEQ GT GEQ
%token AND OR NOT
%token INCREMENT DECREMENT PLUSEQ MINUSEQ 
%token IF ELSE FOR WHILE RETURN FUNCTION 
%token INT BOOL STRING TRUE FALSE
%token RGX RGXEQ RGXNEQ RGXSTRCMP RGXSTRNOT
%token ARRAY MAP EMPTYARR EMPTYMAP 
%token BEGIN LOOP END 
%token CONFIG RS FS NF DOLLAR

%token <int> LITERAL
%token <string> ID
%token EOF

%nonassoc IF ELSE INCREMENT DECREMENT
%right ASSIGN PLUSEQ MINUSEQ
%left OR
%left AND
%left EQUALS NEQ LT LEQ GT GEQ RGXEQ RGXNEQ RGXSTRCMP RGXSTRNOT
%left PLUS MINUS STRCAT
%left TIMES DIVIDE
%right NOT


program: begin_block loop_block end_block EOF

begin_block:
	BEGIN LCURLY func_opts global_var_opts RCURLY {{}}

loop_block:
	LOOP LCURLY expr_opts local_vars_opts RCURLY {{}}

end_block:
	END LCURLY expr_opts local_vars_opts RCURLY {{}}

Config_opts:
	/* nothing */				{[]}
	config_block				{
typ: 
STRING 				{ String }
| INT 					{ Int }
| BOOL					{ Bool }
| ARR					{ Arr }
| MAP					{ Map }
| RGX					{ Rgx }

func_opts:
	/* nothing */				{}
	| func					{ List.rev $1 }

global_vars_opts:
/* nothing */				{}
	| var_decl				{ List.rev $1 }

expr_opts:
	/* nothing */				{}
	| expr					{ List.rev $1 }

local_vars_opts:
	/* nothing */				{}
	| var_decl				{ List.rev $1 }
	
func:
function ID LPAREN formals_opt RPAREN typ LBRACK 

formals_opt:
/* nothing */				{}
	| formals_list			{ List.rev $1 }

formals_list:
	typ ID					{}
	formal_list COMMA typ ID 		{}

var_decl:
	typ ID SEMI { ($1, $2) }



/* STATEMENTS */


stmt_list: 
/* nothing */ { [] } 
| stmt_list stmt { $2 :: $1 } 

stmt: 
expr SEMI { Expr $1 } 
| RETURN expr SEMI { Return $2 } 
| IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) } 
| FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt { For($3, $5, $7, $9) } 
| WHILE LPAREN expr RPAREN stmt { While($3, $5) }

/* EXPRESSIONS */

expr: 
  	LITERAL { Literal($1) } 
| TRUE { BoolLit(true) } 
| FALSE { BoolLit(false) } 
| ID { Id($1) } 
| expr PLUS expr { Binop($1, Add, $3) } 
| expr PLUSEQ expr { Binop($1, Pluseq, $3) } 
| expr STRCAT expr { Binop($1, Strcat, $3) }
| expr MINUS expr { Binop($1, Sub, $3) } 
| expr MINUSEQ expr { Binop($1, Minuseq, $3) } 
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
| NOT expr { Unop(Not, $2) } 
| ID ASSIGN expr { Assign($1, $3) } 
| RS ASSIGN expr { Assign($1, $3) }
| FS ASSIGN expr { Assign($1, $3) }
| LPAREN expr RPAREN { $2 } 
| ID LPAREN actuals_opt RPAREN { Call($1, $3) }

actuals_opt: 
  /* nothing */ { [] } 
| actuals_list { List.rev $1 } 

actuals_list: 
  expr { [$1] } 
| actuals_list COMMA expr { $3 :: $1 }



