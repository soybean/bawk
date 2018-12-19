(* Authors: Ashley, Christine, Melanie, Victoria *)

%{ open Ast %}

%token LPAREN RPAREN LCURLY RCURLY LSQUARE RSQUARE SEMI COMMA
%token PLUS MINUS DIVIDE MULTIPLY ASSIGN STRCAT
%token EQUALS NEQ LT LEQ GT GEQ AND OR NOT
%token PLUSEQ MINUSEQ INCREMENT DECREMENT
%token RGXEQ RGXNEQ RGXSTRCMP RGXSTRNOT
%token RETURN FUNCTION 
%token CONFIG BEGIN LOOP END
%token INT BOOL VOID STRING RGX TRUE FALSE
%token RS FS NF
%token IF ELSE WHILE FOR IN
%token DOLLAR
%token <int> LITERAL
%token <string> STRING_LITERAL
%token <string> RGX_LITERAL
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN PLUSEQ MINUSEQ
%left OR
%left AND
%left EQUALS NEQ RGXEQ RGXNEQ RGXSTRCMP RGXSTRNOT
%left LT LEQ GT GEQ
%left PLUS MINUS STRCAT
%left MULTIPLY DIVIDE
%right NOT NEG
%right INCREMENT DECREMENT 
%left LSQUARE
%right DOLLAR

%%

program: 
  begin_block loop_block end_block config_block EOF { ($1, $2, $3, $4) }

begin_block: 
  BEGIN LCURLY global_vars_list func_list RCURLY { (List.rev $3, List.rev $4) }

loop_block: 
  LOOP LCURLY local_vars_list stmt_list RCURLY { (List.rev $3, List.rev $4) }

end_block: 
  END LCURLY local_vars_list stmt_list RCURLY { (List.rev $3, List.rev $4) }

config_block:							
    /* nothing */                         { [] }
  | CONFIG LCURLY config_expr_list RCURLY { (List.rev $3) }

primitive: 
    STRING { String }
  | INT    { Int }
  | BOOL   { Bool }
  | RGX    { Rgx }

typ: 
    VOID       { Void }
  | array_type { $1 }

array_type: 
    array_type LSQUARE RSQUARE { ArrayType($1) }
  | primitive                  { $1 }

func_list: 		
    /* nothing */  { [] }
  | func_list func { $2 :: $1 }

global_vars_list: 		
    /* nothing */             { [] }
  | global_vars_list var_decl { $2 :: $1 }

local_vars_list:
    /* nothing */            { [] }
  | local_vars_list var_decl { $2 :: $1 }

func: 
  FUNCTION typ ID LPAREN formals_opt RPAREN LCURLY local_vars_list stmt_list RCURLY 
    { { fname = $3; 
        formals = $5; 
        ret_type = $2; 
        locals = List.rev $8;  
        body = List.rev $9 } } 

formals_opt: 	
    /* nothing */ { [] }
  | formals_list  { List.rev $1 }

formals_list: 
    typ ID                    { [($1, $2)] } 
  | formals_list COMMA typ ID { ($3, $4) :: $1 }

var_decl: 
  typ ID SEMI { ($1, $2) }

config_expr_list: 		
    /* nothing */                { [] }
  | config_expr_list config_expr { $2 :: $1 }

config_expr: 
    RS ASSIGN expr SEMI { RSAssign($3) }
  | FS ASSIGN expr SEMI { FSAssign($3) }

stmt_list: 		
    /* nothing */  { [] } 
  | stmt_list stmt { $2 :: $1 } 

stmt: 
    expr SEMI                                             { Expr $1 } 
  | RETURN expr_opt SEMI                                  { Return $2 } 
  | WHILE LPAREN expr RPAREN code_block                   { While($3, $5) }
  | FOR LPAREN expr SEMI expr SEMI expr RPAREN code_block { For($3, $5, $7, $9) }
  | FOR LPAREN ID IN ID RPAREN code_block                 { EnhancedFor($3, $5, $7) }
  | IF LPAREN expr RPAREN code_block %prec NOELSE         { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN code_block ELSE code_block      { If($3, $5, $7) }

code_block: LCURLY stmt_list RCURLY { Block(List.rev $2) }

expr_opt: 
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr: 
    LITERAL                      { Literal($1) } 
  | STRING_LITERAL               { StringLiteral($1) }
  | RGX_LITERAL                  { RgxLiteral($1) }
  | TRUE                         { BoolLit(true) } 
  | FALSE                        { BoolLit(false) } 
  | ID                           { Id($1) } 
  | expr PLUS expr               { Binop($1, Add, $3) } 
  | expr MINUS expr              { Binop($1, Sub, $3) } 
  | expr MULTIPLY expr           { Binop($1, Mult, $3) } 
  | expr DIVIDE expr             { Binop($1, Div, $3) } 
  | expr EQUALS expr             { Binop($1, Equal, $3) } 
  | expr NEQ expr                { Binop($1, Neq, $3) }
  | expr LT expr                 { Binop($1, Less, $3) } 
  | expr LEQ expr                { Binop($1, Leq, $3) } 
  | expr GT expr                 { Binop($1, Greater, $3) } 
  | expr GEQ expr                { Binop($1, Geq, $3) } 
  | expr AND expr                { Binop($1, And, $3) } 
  | expr OR expr                 { Binop($1, Or, $3) }
  | expr PLUSEQ expr             { Pluseq($1, $3) } 
  | expr MINUSEQ expr            { Minuseq($1, $3) }
  | expr INCREMENT               { Increment($1) }
  | expr DECREMENT               { Decrement($1) }
  | expr STRCAT expr             { Strcat($1,$3) }
  | expr RGXEQ expr              { Rgxeq($1, $3) }
  | expr RGXNEQ expr             { Rgxneq($1, $3)}
  | expr RGXSTRCMP expr          { Rgxcomp($1,$3)}
  | expr RGXSTRNOT expr          { Rgxnot($1, $3)}
  | LSQUARE actuals_opt RSQUARE  { ArrayLit($2) }
  | expr LSQUARE expr RSQUARE    { ArrayDeref($1, $3) }
  | NOT expr                     { Unop(Not, $2) }
  | LPAREN expr RPAREN           { $2 } 
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | NF                           { NumFields }
  | DOLLAR expr                  { Access($2) }
  | MINUS expr %prec NEG         { Unop(Neg, $2) }
  | expr ASSIGN expr             { Assign($1, $3) }

actuals_opt: 
    /* nothing */ { [] } 
  | actuals_list  { List.rev $1 } 

actuals_list: 
    expr                    { [$1] } 
  | actuals_list COMMA expr { $3 :: $1 }
