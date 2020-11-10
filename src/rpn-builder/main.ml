open Ast
open Printf

let rec gen_expr_bytecode e =
  match e with
  | Input -> sprintf "OP_INPUT\n"
  | Int i -> sprintf "OP_LOAD_CONST\t%d\n" i
  | Var v -> sprintf "OP_LOAD_VAR\t%s\n" v
  | Binop (b, x, y) -> (
      match b with
      | Add -> gen_expr_bytecode x ^ gen_expr_bytecode y ^ "OP_ADD\n"
      | Mult -> gen_expr_bytecode x ^ gen_expr_bytecode y ^ "OP_MULTIPLY\n"
      | Div -> gen_expr_bytecode x ^ gen_expr_bytecode y ^ "OP_DIVIDE\n"
      | Sub -> gen_expr_bytecode x ^ gen_expr_bytecode y ^ "OP_SUBTRACT\n" )

let gen_assign_bytecode a =
  match a with s, e -> gen_expr_bytecode e ^ sprintf "OP_STORE_VAR\t%s\n" s

let gen_stmt_bytecode s =
  match s with
  | Astmt a -> gen_assign_bytecode a
  | Print a -> gen_expr_bytecode a ^ sprintf "OP_PRINT\n"

let gen_bytecode ast =
  let program = "" in
  List.fold_left (fun acc stmt -> acc ^ gen_stmt_bytecode stmt) program ast

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let parse_file f =
  let input = open_in f in
  let filebuf = Lexing.from_channel input in
  try
    Ok
      (gen_bytecode
         (Parse.parse filebuf (Parser.Incremental.prog filebuf.lex_curr_p)))
  with
  | Lexer.Error msg -> Error (sprintf "%s!" msg)
  | Parser.Error ->
      Error
        (sprintf "At offset %d: syntax errorn.\n%!"
           (Lexing.lexeme_start filebuf))
  | Util.Syntax_error (location, msg) -> (
      match location with
      | Some (line, pos) ->
          Error (sprintf "Syntax error at %d:%d\n%s" line pos msg)
      | None -> Error (sprintf "%s" msg) )

let write_file s =
  let oc = open_out_bin "a.avb" in
  fprintf oc "%s" s;
  close_out oc

let () =
  if Array.length Sys.argv == 2 then (
    let test = Sys.argv.(1) in
    ignore print_newline;
    let bc = parse_file test in
    match bc with
    | Ok x -> write_file x
    | Error e ->
        eprintf "%s" e;
        exit (-1) )
