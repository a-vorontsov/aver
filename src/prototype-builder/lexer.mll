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
let letter = ['a'-'z' 'A'-'Z']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token =
  parse
  | white { token lexbuf }
  | newline { next_line lexbuf; token lexbuf }
  | "/" { DIV }
  | "-" { MINUS }
  | "*" { TIMES }
  | "+" { PLUS }
  | "%" { MOD }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "=" { EQUALS }
  | "==" { BEQUALS }
  | "!=" { BNEQUALS }
  | "<" { LT }
  | ">" { GT }
  | ";" { SEMICOLON }
  | "," { COMMA }
  | "print" { PRINT }
  | "input" { INPUT }
  | "while" { WHILE }
  | "if" { IF }
  | "else" { ELSE }
  | "pass" { PASS }
  | "func" { FUNC }
  | "return" { RETURN }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
