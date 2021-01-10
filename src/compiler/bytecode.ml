open Ast
open Instruction
open Table

let functions_table = Hashtbl.create 32

let functions_counter = ref 0

let new_function () =
  incr functions_counter;
  !functions_counter

let insert_function f p =
  if f = "main" then 0
  else
    let func = Hashtbl.find_opt functions_table (f, p) in
    match func with
    | Some func' -> func'
    | None ->
        let func' = new_function () in
        Hashtbl.add functions_table (f, p) func';
        func'

let confirm_function f p =
  let func = Hashtbl.find_opt functions_table (f, p) in
  match func with Some func' -> func' | None -> assert false

let rec append_bc code bytecode =
  match bytecode with [] -> [ code ] | h :: t -> h :: append_bc code t

let rec gen_expr_bytecode expression vars_table bytecode =
  match expression with
  | Input -> append_bc INPUT bytecode
  | Num i -> append_bc (LOAD_CONST i) bytecode
  | Var v -> append_bc (LOAD_VAR (vars_table#get v)) bytecode
  | AssignCall (name, params) ->
      gen_call_bytecode name params vars_table bytecode
  | Binop (op, x, y) ->
      gen_expr_bytecode x vars_table bytecode
      @ gen_expr_bytecode y vars_table bytecode
      @ append_bc
          ( match op with
          | Add -> ADD
          | Mult -> MULTIPLY
          | Div -> DIVIDE
          | Mod -> MOD
          | Sub -> SUBTRACT )
          bytecode

and gen_assign_bytecode assignment vars_table bytecode =
  match assignment with
  | name, expression ->
      if vars_table#exists name then
        gen_expr_bytecode expression vars_table bytecode
        @ append_bc (STORE_VAR (vars_table#get name)) bytecode
      else assert false

and gen_declare_bytecode declaration vars_table bytecode =
  match declaration with
  | name, _, expression -> (
      match expression with
      | None ->
          append_bc (LOAD_CONST 0) bytecode
          @ append_bc (STORE_VAR (vars_table#insert name)) bytecode
      | Some e ->
          gen_expr_bytecode e vars_table bytecode
          @ append_bc (STORE_VAR (vars_table#insert name)) bytecode )

and gen_condition_bytecode condition jump_to vars_table bytecode =
  match condition with
  | Bincond (op, expression, expression') ->
      gen_expr_bytecode expression vars_table bytecode
      @ gen_expr_bytecode expression' vars_table bytecode
      @ append_bc
          ( match op with
          | BEquals -> CMPEQ jump_to
          | BNequals -> CMPNEQ jump_to
          | GreaterThan -> CMPGT jump_to
          | LessThan -> CMPLT jump_to )
          bytecode

and gen_while_bytecode condition statement_list vars_table bytecode =
  let stmts =
    List.fold_left
      (fun acc stmt -> acc @ gen_stmt_bytecode stmt vars_table [])
      bytecode statement_list
  in
  let condition =
    gen_condition_bytecode condition (List.length stmts + 1) vars_table bytecode
  in
  condition @ stmts
  @ append_bc (JUMP (-(List.length stmts + List.length condition + 1))) bytecode

and gen_if_bytecode condition statements statements' vars_table bytecode =
  let stmts_if =
    List.fold_left
      (fun acc stmt -> acc @ gen_stmt_bytecode stmt vars_table [])
      bytecode statements
  in
  let stmts_else =
    List.fold_left
      (fun acc stmt -> acc @ gen_stmt_bytecode stmt vars_table [])
      bytecode statements'
  in
  let condition =
    gen_condition_bytecode condition
      (List.length stmts_if + 1)
      vars_table bytecode
  in
  condition @ stmts_if
  @ append_bc (JUMP (List.length stmts_else)) bytecode
  @ stmts_else

and gen_call_bytecode name params vars_table bytecode =
  List.fold_left
    (fun acc expr -> acc @ gen_expr_bytecode expr vars_table [])
    bytecode (List.rev params)
  @ append_bc
      (CALL (confirm_function name (List.length params), List.length params))
      bytecode

and gen_stmt_bytecode statement vars_table bytecode =
  match statement with
  | Declare declaration -> gen_declare_bytecode declaration vars_table bytecode
  | Assign assignment -> gen_assign_bytecode assignment vars_table bytecode
  | Print expression ->
      gen_expr_bytecode expression vars_table bytecode
      @ append_bc PRINT bytecode
  | If (condition, statements, statements') ->
      gen_if_bytecode condition statements statements' vars_table bytecode
  | While (condition, statements) ->
      gen_while_bytecode condition statements vars_table bytecode
  | Call (name, params) -> gen_call_bytecode name params vars_table bytecode
  | Return expression ->
      gen_expr_bytecode expression vars_table bytecode
      @ append_bc RETURN bytecode
  | Pass -> append_bc PASS bytecode

and gen_func_bytecode func vars_table bytecode =
  match func with
  | Func (name, params, block) ->
      List.iter (fun (param, _) -> ignore (vars_table#insert param)) params;
      let function_name = insert_function name (List.length params) in
      append_bc (MAKE_FUNCTION (function_name, List.length params)) bytecode
      @ List.fold_left
          (fun acc stmt -> acc @ gen_stmt_bytecode stmt vars_table [])
          bytecode block
      @ append_bc HALT bytecode

and gen_functions ast bytecode =
  List.fold_left
    (fun acc funcs -> acc @ gen_func_bytecode funcs (new table) [])
    bytecode ast

and gen_bytecode ast = gen_functions ast [] @ [ CALL (0, 0) ]
