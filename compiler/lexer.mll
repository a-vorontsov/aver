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
let struct_id = ['A'-'Z'] ['a'-'z' 'A'-'Z' '_']+
let generic_type = ['A'-'Z']

rule read_token =
  parse
  | white { read_token lexbuf }
  | newline { next_line lexbuf; read_token lexbuf }
  | "//" { read_comment lexbuf }
  | "/" { DIV }
  | "-" { MINUS }
  | "*" { TIMES }
  | "+" { PLUS }
  | "%" { MOD }
  | "&&" { BAND }
  | "||" { BOR }
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
  | ">=" { GE }
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
  | "~" { TILDE }
  | struct_id { STRUCT_ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | id { ID (Lexing.lexeme lexbuf) }
  | '"' { read_string (Buffer.create 16) lexbuf }
  | eof { EOF }
  | _ { raise (Error (Printf.sprintf "At offset %d: Unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

and read_comment = parse
  | newline { next_line lexbuf; read_token lexbuf }
  | eof { EOF }
  | _ { read_comment lexbuf }

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | [^ '"']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | eof { raise (Error ("EOF: String has not been terminated.")) }
  | _ { raise (Error (Printf.sprintf "At offset %d: Illegal character.\n" (Lexing.lexeme_start lexbuf))) }
