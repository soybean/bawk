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
| "<"        { LTRI }
| ">"        { RTRI }
| ";"        { SEMI }
| "&"        { STRCAT }
| ","        { COMMA }
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
| "+="       { PLUSEQ }
| "-="       { MINUSEQ }
| "++"       { INCREMENT }
| "--"       { DECREMENT }
| "if"       { IF }
| "else"     { ELSE }
| "while"    { WHILE }
| "!"        { NOT }
| "return"   { RETURN }
| "function" { FUNCTION }
| "int"      { INT }
| "bool"     { BOOL }
| "void"     { VOID }
| "string"   { STRING }
| "rgx"      { RGX }
| "%"        { RGXEQ } 
| "!%"       { RGXNEQ }
| "~"        { RGXSTRCMP }
| "!~"       { RGXSTRNOT }
| "int[]"    { INTARR }
| "string[]" { STRINGARR }
| "bool[]"   { BOOLARR }
| "rgx[]"    { RGXARR }
| "[]"       { EMPTYARR }
| "{}"       { EMPTYMAP }
| "map"      { MAP }
| "CONFIG"   { CONFIG }
| "RS"       { RS }
| "FS"       { FS }
| "BEGIN"    { BEGIN }
| "LOOP"     { LOOP }
| "END"      { END }
| "true"     { TRUE }
| "false"    { FALSE }
(*| ["0"-"9"]+ as lxm { LITERAL(int_of_string lxm) }*)
(*| ["a"-"z" "A"-"Z"]["a"-"z" "A"-"Z" "0"-"9" "_"]* as lxm { ID(lxm) }*)
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^
                              Char.escaped char)) }

and comment = parse
  '\n' { token lexbuf }
| _    { comment lexbuf }
