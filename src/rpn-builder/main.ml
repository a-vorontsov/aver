open Ast
open Printf

let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let parse_file (f : string) : prog =
  let input = open_in f in
  let filebuf = Lexing.from_channel input in
  let ast = Parser.prog Lexer.read filebuf in
  ast

let rec gen_expr_bytecode (e : expr) =
  match e with
  | Input -> sprintf "INPUT\n"
  | Int i -> sprintf "LOAD_CONST %d\n" i
  | Var v -> sprintf "LOAD_VAR %s\n" v
  | Binop (b, x, y) -> (
      match b with
      | Add -> gen_expr_bytecode x ^ gen_expr_bytecode y ^ "ADD\n"
      | Mult -> gen_expr_bytecode x ^ gen_expr_bytecode y ^ "MULT\n"
      | Div -> gen_expr_bytecode x ^ gen_expr_bytecode y ^ "DIV\n"
      | Sub -> gen_expr_bytecode x ^ gen_expr_bytecode y ^ "SUB\n" )

let gen_assign_bytecode (a : assignment) =
  match a with s, e -> gen_expr_bytecode e ^ sprintf "STORE_VAR %s\n" s

let gen_stmt_bytecode (s : stmt) =
  match s with
  | Astmt a -> gen_assign_bytecode a
  | Print a -> gen_expr_bytecode a ^ sprintf "PRINT\n"

let rec gen_bytecode (ast : prog) =
  let program = "" in
  List.fold_left (fun acc stmt -> acc ^ gen_stmt_bytecode stmt) program ast

let write_file (s : string) =
  let oc = open_out "a.avb" in
  fprintf oc "%s" s;
  close_out oc

let () =
  if Array.length Sys.argv == 2 then (
    let test = Sys.argv.(1) in
    ignore print_newline;
    let bc = gen_bytecode (parse_file test) in
    write_file bc )
