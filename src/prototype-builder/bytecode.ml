open Ast
open Instruction

let vars_table = Hashtbl.create 32

let var_counter = ref 0

let new_var () =
  incr var_counter;
  !var_counter

let replace_var op =
  let var = Hashtbl.find_opt vars_table op in
  match var with
  | Some var' -> var'
  | None ->
      let var' = new_var () in
      Hashtbl.add vars_table op var';
      var'

let rec append_bc c bc =
  match bc with [] -> [ c ] | h :: t -> h :: append_bc c t

let rec gen_expr_bytecode e b =
  match e with
  | Input -> append_bc INPUT b
  | Int i -> append_bc (LOAD_CONST i) b
  | Var v -> append_bc (LOAD_VAR (replace_var v)) b
  | Binop (o, x, y) ->
      gen_expr_bytecode x b @ gen_expr_bytecode y b
      @ append_bc
          ( match o with
          | Add -> ADD
          | Mult -> MULTIPLY
          | Div -> DIVIDE
          | Mod -> MOD
          | Sub -> SUBTRACT )
          b

let gen_assign_bytecode a b =
  match a with
  | s, e -> gen_expr_bytecode e b @ append_bc (STORE_VAR (replace_var s)) b

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

and gen_if_bytecode c s s' b =
  let stmts_if =
    List.fold_left (fun acc stmt -> acc @ gen_stmt_bytecode stmt []) b s
  in
  let stmts_else =
    List.fold_left (fun acc stmt -> acc @ gen_stmt_bytecode stmt []) b s'
  in
  let condition = gen_condition_bytecode c (List.length stmts_if + 1) b in
  condition @ stmts_if
  @ append_bc (JUMP (List.length stmts_else)) b
  @ stmts_else

and gen_stmt_bytecode s b =
  match s with
  | Assign a -> gen_assign_bytecode a b
  | Print a -> gen_expr_bytecode a b @ append_bc PRINT b
  | If (c, s, s') -> gen_if_bytecode c s s' b
  | While (c, s) -> gen_while_bytecode c s b
  | Pass -> append_bc PASS b

let gen_bytecode a =
  List.fold_left (fun acc stmt -> acc @ gen_stmt_bytecode stmt []) [] a
