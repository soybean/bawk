{ open Parser }

rule token = parse 
[' ' '\t' '\r' '\n'] 
{ token lexbuf } (* Whitespace *)
| "#"       { comment lexbuf } (* Comments *)
| "("       { LPAREN }
| ")"       { RPAREN }
| "{"       { LBRACKET }
| "}"       { RBRACKET }
| "["       { LBRACE }
| "]"       { RBRACE }
| ";"       { SEMI }
| ","       { COMMA }
| "&"       { STRCAT }
| "+"       { PLUS }
| "-"       { MINUS }
| "/"       { DIVIDE }
| "="       { ASSIGN }
| "=="      { EQUALS }
| "!="      { NEQ }
| "<"       { LT }
| "<="      { LEQ }
| ">"       { GT }
| ">="      { GEQ }
| "&&"      { AND }
| "||"      { OR }
| "!"       { NOT }
| "++"      { INCREMENT }
| "--"      { DECREMENT }
| "+="      { PLUSEQ }
| "-="      { MINUSEQ }
| "$"       { DOLLAR }
| "if"      { IF }
| "else"    { ELSE }
| "for"     { FOR }
| "while"   { WHILE }
| "return"  { RETURN }
| "function" { FUNCTION }
| "int"     { INT }
| "bool"    { BOOL }
| "char"    { CHAR }
| "string"  { STRING }
| "rgx"     { RGX }
| "%"       { RGXCOMP } 
| "!~"      { RGXNEQ }
| "~"       { RGXEQ }
| "arr[]"   { ARRAY }
| "map"     { MAP }
| "[]"      { EMPTYARR }
| "{}"      { EMPTYMAP }
| "BEGIN"   { BEGIN }
| "LOOP"    { LOOP }
| "END"     { END }
| "CONFIG"  { CONFIG }
| "RS"      { RS }
| "FS"      { FS }
| "NF"      { NF }
| "true"    { TRUE }
| "false"   { FALSE }
| ["0"-"9"]+ as lxm { LITERAL(int_of_string lxm) }
| ["a"-"z" "A"-"Z"]["a"-"z" "A"-"Z" "0"-"9" "_"]* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^
Char.escaped char)) }


| ’+’ { PLUS }
| ’-’ { MINUS }
| ’*’ { TIMES }
| ’/’ { DIVIDE }
| [’0’-’9’]+ as lit { LITERAL(int_of_string lit) }
| eof { EOF }
