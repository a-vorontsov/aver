open Ast
open Printf
open Instruction

let counter = ref 0

let new_label () =
  incr counter;
  !counter

let rec append_bc c bc =
  match bc with [] -> [ c ] | h :: t -> h :: append_bc c t

let rec gen_expr_bytecode e b =
  match e with
  | Input -> append_bc INPUT b
  | Int i -> append_bc (LOAD_CONST i) b
  | Var v -> append_bc (LOAD_VAR v) b
  | Binop (o, x, y) -> (
      match o with
      | Add -> gen_expr_bytecode x b @ gen_expr_bytecode y b @ append_bc ADD b
      | Mult ->
        gen_expr_bytecode x b @ gen_expr_bytecode y b @ append_bc MULTIPLY b
      | Div ->
        gen_expr_bytecode x b @ gen_expr_bytecode y b @ append_bc DIVIDE b
      | Sub ->
        gen_expr_bytecode x b @ gen_expr_bytecode y b @ append_bc SUBTRACT b )

let gen_assign_bytecode a b =
  match a with s, e -> gen_expr_bytecode e b @ append_bc (STORE_VAR s) b

let gen_condition_bytecode c j b =
  match c with
  | Bincond (o, e, e') -> (
      match o with
      | BEquals ->
        gen_expr_bytecode e b @ gen_expr_bytecode e' b @ append_bc (CMPEQ j) b
      | BNequals ->
        gen_expr_bytecode e b @ gen_expr_bytecode e' b
        @ append_bc (CMPNEQ j) b
      | GreaterThan ->
        gen_expr_bytecode e b @ gen_expr_bytecode e' b @ append_bc (CMPLT j) b
      | LessThan ->
        gen_expr_bytecode e b @ gen_expr_bytecode e' b @ append_bc (CMPGT j) b
    )

let rec gen_while_bytecode c sl b =
  let stmts =
    List.fold_left (fun acc stmt -> acc @ gen_stmt_bytecode stmt []) b sl
  in
  let condition = gen_condition_bytecode c (List.length stmts + 1) b in
  condition @ stmts
  @ append_bc (JUMP (-(List.length stmts + List.length condition + 1))) b

and gen_stmt_bytecode s b =
  match s with
  | Assign a -> gen_assign_bytecode a b
  | Print a -> gen_expr_bytecode a b @ append_bc PRINT b
  | While (c, s) -> gen_while_bytecode c s b

let gen_bytecode a =
  List.fold_left (fun acc stmt -> acc @ gen_stmt_bytecode stmt []) [] a

let emit b = List.fold_left (fun acc stmt -> acc ^ as_string stmt ^ "\n") "" b

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.token lexbuf in
  ast

let parse_file f =
  let input = open_in f in
  let filebuf = Lexing.from_channel input in
  try
    Ok
      (emit
         (gen_bytecode
            (Parse.parse filebuf (Parser.Incremental.prog filebuf.lex_curr_p))))
  with
  | Lexer.Error msg -> Error (sprintf "%s!" msg)
  | Parser.Error ->
    Error
      (sprintf "Syntax error at offset %d:\n%!" (Lexing.lexeme_start filebuf))
  | Util.Syntax_error (location, msg) -> (
      match location with
      | Some (line, pos) ->
        Error (sprintf "Syntax error at %d:%d\n%s" line pos msg)
      | None -> Error (sprintf "%s" msg) )

let write_file s out =
  let oc = open_out_bin (out ^ ".avb") in
  let b = Bytes.of_string s in
  output_bytes oc b;
  close_out oc

let () =
  if Array.length Sys.argv == 2 then (
    let file = Sys.argv.(1) in
    ignore print_newline;
    let bc = parse_file file in
    match bc with
    | Ok x -> write_file x (Filename.remove_extension file)
    | Error e ->
      eprintf "%s" e;
      exit (-1) )
