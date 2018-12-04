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
| '"'        { read_string (Buffer.create 17) lexbuf }
| "'"        { read_rgx (Buffer.create 17) lexbuf }
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
| "\'" [^'\'']* "\'" as lxm { RGX_LITERAL(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  '\n' { token lexbuf }
| _    { comment lexbuf }

and read_string buf =
  parse
  | '"'       { STRING_LITERAL (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Failure ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Failure ("String is not terminated")) }

and read_rgx buf =
  parse
  | '''       { RGX_LITERAL (Buffer.contents buf) }
  | [^ ''']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Failure ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Failure ("String is not terminated")) }

