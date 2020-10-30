open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec eval (e : expr) =
  match e with
  | Int i -> i
  | Binop (b, x, y) -> (
      match b with
      | Add -> eval x + eval y
      | Mult -> eval x * eval y
      | Div -> eval x / eval y
      | Sub -> eval x - eval y )

let rec gen_bytecode (ast : expr) =
  match ast with
  | Int i -> Printf.sprintf "LOAD %d\n" i
  | Binop (b, x, y) -> (
      match b with
      | Add -> gen_bytecode x ^ gen_bytecode y ^ "ADD\n"
      | Mult -> gen_bytecode x ^ gen_bytecode y ^ "MULT\n"
      | Div -> gen_bytecode x ^ gen_bytecode y ^ "DIV\n"
      | Sub -> gen_bytecode x ^ gen_bytecode y ^ "SUB\n" )

let write_file (s : string) =
  let oc = open_out "a.avb" in
  Printf.fprintf oc "%s" s;
  close_out oc

let () =
  if Array.length Sys.argv == 2 then (
    let test = Sys.argv.(1) in
    ignore print_newline;
    let bc = gen_bytecode (parse test) in
    write_file bc )
