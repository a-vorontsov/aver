open Lexing

let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "line:%d column:%d" pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse lexbuf =
  try Parser.prog Lexer.token lexbuf with
  | Lexer.Error msg ->
      Printf.eprintf "Lexing Error at %s: %s\n"
        (print_error_position lexbuf)
        msg;
      exit (-1)
  | Parser.Error ->
      Printf.eprintf "Syntax Error at %s\n" (print_error_position lexbuf);
      exit (-1)
