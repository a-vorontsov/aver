open Ast
open Instruction

let vars_table = Hashtbl.create 32

let var_counter = ref 0

let new_var () =
  incr var_counter;
  !var_counter

let functions_table = Hashtbl.create 32

let functions_counter = ref 0

let new_function () =
  incr functions_counter;
  !functions_counter

let insert_var name =
  let var = Hashtbl.find_opt vars_table name in
  match var with
  | Some var' -> var'
  | None ->
      let var' = new_var () in
      Hashtbl.add vars_table name var';
      var'

let insert_function f =
  if f = "main" then 0
  else
    let func = Hashtbl.find_opt functions_table f in
    match func with
    | Some func' -> func'
    | None ->
        let func' = new_function () in
        Hashtbl.add functions_table f func';
        func'

let rec append_bc code bytecode =
  match bytecode with [] -> [ code ] | h :: t -> h :: append_bc code t

let rec gen_expr_bytecode expression bytecode =
  match expression with
  | Input -> append_bc INPUT bytecode
  | Int i -> append_bc (LOAD_CONST i) bytecode
  | Var v -> append_bc (LOAD_VAR (insert_var v)) bytecode
  | AssignCall (name, _) -> gen_call_bytecode name bytecode
  | Binop (op, x, y) ->
      gen_expr_bytecode x bytecode
      @ gen_expr_bytecode y bytecode
      @ append_bc
          ( match op with
          | Add -> ADD
          | Mult -> MULTIPLY
          | Div -> DIVIDE
          | Mod -> MOD
          | Sub -> SUBTRACT )
          bytecode

and gen_assign_bytecode assignment bytecode =
  match assignment with
  | name, expression ->
      gen_expr_bytecode expression bytecode
      @ append_bc (STORE_VAR (insert_var name)) bytecode

and gen_condition_bytecode condition jump_to bytecode =
  match condition with
  | Bincond (op, expression, expression') ->
      gen_expr_bytecode expression bytecode
      @ gen_expr_bytecode expression' bytecode
      @ append_bc
          ( match op with
          | BEquals -> CMPEQ jump_to
          | BNequals -> CMPNEQ jump_to
          | GreaterThan -> CMPGT jump_to
          | LessThan -> CMPLT jump_to )
          bytecode

and gen_while_bytecode condition statement_list bytecode =
  let stmts =
    List.fold_left
      (fun acc stmt -> acc @ gen_stmt_bytecode stmt [])
      bytecode statement_list
  in
  let condition =
    gen_condition_bytecode condition (List.length stmts + 1) bytecode
  in
  condition @ stmts
  @ append_bc (JUMP (-(List.length stmts + List.length condition + 1))) bytecode

and gen_if_bytecode condition statements statements' bytecode =
  let stmts_if =
    List.fold_left
      (fun acc stmt -> acc @ gen_stmt_bytecode stmt [])
      bytecode statements
  in
  let stmts_else =
    List.fold_left
      (fun acc stmt -> acc @ gen_stmt_bytecode stmt [])
      bytecode statements'
  in
  let condition =
    gen_condition_bytecode condition (List.length stmts_if + 1) bytecode
  in
  condition @ stmts_if
  @ append_bc (JUMP (List.length stmts_else)) bytecode
  @ stmts_else

and gen_call_bytecode name bytecode =
  append_bc (CALL (insert_function name)) bytecode

and gen_stmt_bytecode statement bytecode =
  match statement with
  | Assign assignment -> gen_assign_bytecode assignment bytecode
  | Print expression ->
      gen_expr_bytecode expression bytecode @ append_bc PRINT bytecode
  | If (condition, statements, statements') ->
      gen_if_bytecode condition statements statements' bytecode
  | While (condition, statements) ->
      gen_while_bytecode condition statements bytecode
  | Call (name, _) -> gen_call_bytecode name bytecode
  | Return expression ->
      gen_expr_bytecode expression bytecode @ append_bc RETURN bytecode
  | Pass -> append_bc PASS bytecode

and gen_func_bytecode func bytecode =
  match func with
  | Func (name, _, block) ->
      append_bc (MAKE_FUNCTION (insert_function name)) bytecode
      @ List.fold_left
          (fun acc stmt -> acc @ gen_stmt_bytecode stmt [])
          bytecode block
      @ append_bc HALT bytecode

and gen_functions ast bytecode =
  List.fold_left
    (fun acc funcs -> acc @ gen_func_bytecode funcs [])
    bytecode ast

and gen_bytecode ast = gen_functions ast [] @ [ CALL 0 ]
