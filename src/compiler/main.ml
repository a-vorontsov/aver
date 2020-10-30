open Ast

let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let parse_file (f : string) : prog =
  let input = open_in f in
  let filebuf = Lexing.from_channel input in
  let ast = Parser.prog Lexer.read filebuf in
  ast
