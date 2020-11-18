open Ast
open Instruction

let rec append_bc c bc =
  match bc with [] -> [ c ] | h :: t -> h :: append_bc c t

let rec gen_expr_bytecode e b =
  match e with
  | Input -> append_bc INPUT b
  | Int i -> append_bc (LOAD_CONST i) b
  | Var v -> append_bc (LOAD_VAR v) b
  | Binop (o, x, y) ->
      gen_expr_bytecode x b @ gen_expr_bytecode y b
      @ append_bc
          ( match o with
          | Add -> ADD
          | Mult -> MULTIPLY
          | Div -> DIVIDE
          | Sub -> SUBTRACT )
          b

let gen_assign_bytecode a b =
  match a with s, e -> gen_expr_bytecode e b @ append_bc (STORE_VAR s) b

let gen_condition_bytecode c j b =
  match c with
  | Bincond (o, e, e') ->
      gen_expr_bytecode e b @ gen_expr_bytecode e' b
      @ append_bc
          ( match o with
          | BEquals -> CMPEQ j
          | BNequals -> CMPNEQ j
          | GreaterThan -> CMPGT j
          | LessThan -> CMPLT j )
          b

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
