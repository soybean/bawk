{ open Parser }

rule token = parse 
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "#"        { comment lexbuf } (* Comments *)
| "("        { LPAREN }
| ")"        { RPAREN }
| "{"        { LCURLY }
| "}"        { RCURLY }
| "["        { LSQUARE }
| "]"        { RSQUARE }
| ";"        { SEMI }
| "&"        { STRCAT }
| "$"        { DOLLAR }
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
| "for"      { FOR }
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
| "CONFIG"   { CONFIG }
| "RS"       { RS }
| "FS"       { FS }
| "NF"       { NF }
| "BEGIN"    { BEGIN }
| "LOOP"     { LOOP }
| "END"      { END }
| "true"     { TRUE }
| "false"    { FALSE }
| "in"       { IN }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| "\"" [^'\"']* "\"" as lxm { STRING_LITERAL(lxm) }
| "\'" [^'\'']* "\'" as lxm { RGX_LITERAL(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  '\n' { token lexbuf }
| _    { comment lexbuf }
