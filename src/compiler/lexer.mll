{
  open Lexing
  open Parser

  exception Error of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <-
        { pos with pos_bol = lexbuf.lex_curr_pos;
                   pos_lnum = pos.pos_lnum + 1
        }
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let int = '-'? digit+
let float = '-'? digit+ '.' digit+
let letter = ['a'-'z' 'A'-'Z']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let struct_id = ['A'-'Z'] ['a'-'z' 'A'-'Z' '_']*
let generic_type = ['A'-'Z']

rule token =
  parse
  | white { token lexbuf }
  | newline { next_line lexbuf; token lexbuf }
  | "//" { read_single_line_comment lexbuf }
  | "/" { DIV }
  | "-" { MINUS }
  | "*" { TIMES }
  | "+" { PLUS }
  | "%" { MOD }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LSQUARE }
  | "]" { RSQUARE }
  | "=" { EQUALS }
  | "==" { BEQUALS }
  | "!=" { BNEQUALS }
  | "<" { LT }
  | "<=" { LE }
  | ">" { GT }
  | ">=" { LE }
  | ":" { COLON }
  | ";" { SEMICOLON }
  | "," { COMMA }
  | "." { DOT }
  | "let" { LET }
  | "print" { PRINT }
  | "println" { PRINTLN }
  | "input" { INPUT }
  | "while" { WHILE }
  | "if" { IF }
  | "else" { ELSE }
  | "pass" { PASS }
  | "func" { FUNC }
  | "return" { RETURN }
  | "int" { T_INT }
  | "float" { T_FLOAT }
  | "bool" { T_BOOL }
  | "char" { T_CHAR }
  | "string" { T_STRING }
  | "void" { T_VOID }
  | generic_type { T_GENERIC }
  | "struct" { STRUCT }
  | "null" { NULL }
  | struct_id { STRUCT_ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | id { ID (Lexing.lexeme lexbuf) }
  | '"' { read_string (Buffer.create 16) lexbuf }
  | eof { EOF }
  | _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

and read_single_line_comment = parse
  | newline { next_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ { read_single_line_comment lexbuf }

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
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
  | eof { raise (Error ("String is not terminated")) }
  | _ { raise (Error (Printf.sprintf "Illegal string character: at %d\n" (Lexing.lexeme_start lexbuf))) }
