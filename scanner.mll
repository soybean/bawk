{ open Parser }

rule token = parse 
[' ' '\t' '\r' '\n'] 
{ token lexbuf } (* Whitespace *)
| "#"        { comment lexbuf } (* Comments *)
| "("        { LPAREN }
| ")"        { RPAREN }
| "{"        { LCURLY }
| "}"        { RCURLY }
| "["        { LSQUARE }
| "]"        { RSQUARE }
| ";"        { SEMI }
| ","        { COMMA }
| "&"        { STRCAT }
| "+"        { PLUS }
| "-"        { MINUS }
| "/"        { DIVIDE }
| "*"        { MULTIPLY }
| "="        { ASSIGN }
| "=="       { EQUALS }
| "!="       { NEQ }
| "<"        { LT }
| "<="       { LEQ }
| ">"        { GT }
| ">="       { GEQ }
| "&&"       { AND }
| "||"       { OR }
| "!"        { NOT }
| "++"       { INCREMENT }
| "--"       { DECREMENT }
| "+="       { PLUSEQ }
| "-="       { MINUSEQ }
| "$"        { DOLLAR }
| "if"       { IF }
| "else"     { ELSE }
| "for"      { FOR }
| "while"    { WHILE }
| "return"   { RETURN }
| "function" { FUNCTION }
| "int"      { INT }
| "bool"     { BOOL }
| "string"   { STRING }
| "rgx"      { RGX }
| "%"        { RGXEQ } 
| "!%"       { RGXNEQ }
| "~"        { RGXSTRCMP }
| "!~"       { RGXSTRNOT }
| "arr[]"    { ARRAY }
| "map"      { MAP }
| "[]"       { EMPTYARR }
| "{}"       { EMPTYMAP }
| "BEGIN"    { BEGIN }
| "LOOP"     { LOOP }
| "END"      { END }
| "CONFIG"   { CONFIG }
| "RS"       { RS }
| "FS"       { FS }
| "NF"       { NF }
| "true"     { TRUE }
| "false"    { FALSE }
| ["0"-"9"]+ as lxm { LITERAL(int_of_string lxm) }
| ["a"-"z" "A"-"Z"]["a"-"z" "A"-"Z" "0"-"9" "_"]* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^
                              Char.escaped char)) }

and comment = parse
  '\n' { token lexbuf }
| _    { comment lexbuf }
