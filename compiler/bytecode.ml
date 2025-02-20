open Tast
open Instruction
open Table
open Types

let rec findi x lst acc =
  match lst with
  | [] ->
      Printf.eprintf "Not found";
      exit (-1)
  | h :: t -> if x = h then acc else findi x t (1 + acc)

let structs_table = Hashtbl.create 32

let fields_to_array fields =
  List.map (fun (TStructField (_, n, t)) -> (n, t)) fields

let insert_struct (TStruct (_, name, _, f)) =
  Hashtbl.add structs_table name (fields_to_array f)

let get_struct name =
  let s = Hashtbl.find_opt structs_table name in
  match s with
  | Some s' -> s'
  | None ->
      Printf.eprintf "Unable to find struct in table";
      exit (-1)

let get_struct_field name field =
  let fields = get_struct name in
  findi field (List.map (fun (f, _) -> f) fields) 0

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
  match func with Some func' -> func' | None -> exit (-1)

let append_bc code bytecode = bytecode @ [ code ]

let rec gen_struct_field_getters fields struct_name bytecode =
  match fields with
  | [] ->
      Printf.eprintf "Unable to get empty field";
      exit (-1)
  | [ (field, _) ] ->
      append_bc (GET_FIELD (get_struct_field struct_name field)) bytecode
  | (field, field_type) :: fields -> (
      match field_type with
      | T_obj (name, _) ->
          append_bc (GET_FIELD (get_struct_field struct_name field)) bytecode
          @ gen_struct_field_getters fields name bytecode
      | _ ->
          Printf.eprintf "Unable to chain non-struct field";
          exit (-1) )

let rec gen_struct_field_setters fields struct_name bytecode =
  match fields with
  | [] ->
      Printf.eprintf "Unable to set empty field";
      exit (-1)
  | [ (field, _) ] ->
      append_bc (SET_FIELD (get_struct_field struct_name field)) bytecode
  | (_, field_type) :: fields -> (
      match field_type with
      | T_obj (name, _) -> gen_struct_field_setters fields name bytecode
      | _ ->
          Printf.eprintf "Unable to chain non-struct field";
          exit (-1) )

let rec gen_identifier_bytecode identifier vars_table bytecode =
  match identifier with
  | TVar (v, _) -> append_bc (LOAD_VAR (vars_table#get v)) bytecode
  | TObjField (v, t, fields) -> (
      match t with
      | T_obj (name, _) ->
          append_bc (LOAD_VAR (vars_table#get v)) bytecode
          @ gen_struct_field_getters fields name bytecode
      | _ ->
          Printf.eprintf "Unable to get field from non-object value";
          exit (-1) )

and gen_array_access_bytecode (v, exprs) vars_table bytecode =
  gen_identifier_bytecode v vars_table bytecode
  @ List.fold_left
      (fun acc e ->
        acc @ gen_expr_bytecode e vars_table [] @ append_bc LOAD_FROM_ARRAY [])
      [] exprs

and gen_expr_bytecode expression vars_table bytecode =
  match expression with
  | TNull _ -> append_bc (LOAD_CONST 0) bytecode
  | TInput _ -> append_bc INPUT bytecode
  | TNum (_, i) -> append_bc (LOAD_CONST_I i) bytecode
  | TFNum (_, f) -> append_bc (LOAD_CONST_F f) bytecode
  | TBool (_, b) -> append_bc (LOAD_CONST_B b) bytecode
  | TStr (_, s) -> append_bc (LOAD_CONST_S s) bytecode
  | TIdentifier (_, v) -> gen_identifier_bytecode v vars_table bytecode
  | TArray (_, _, elements) ->
      List.fold_left
        (fun acc elem -> acc @ gen_expr_bytecode elem vars_table [])
        bytecode (List.rev elements)
      @ append_bc (MAKE_ARRAY (List.length elements)) bytecode
  | TArrayAccess (_, _, a) -> gen_array_access_bytecode a vars_table bytecode
  | TArrayDec (_, _, dec) -> gen_arr_dec_bytecode dec bytecode
  | TAssignCall (_, (name, _, params)) ->
      gen_call_bytecode name params vars_table bytecode
  | TBinop (_, t, op, x, y) ->
      gen_expr_bytecode x vars_table bytecode
      @ gen_expr_bytecode y vars_table bytecode
      @ append_bc
          ( match op with
          | TAdd -> (
              match t with T_int | T_float | T_string -> ADD | _ -> exit (-1) )
          | TMult -> (
              match t with T_int | T_float -> MULTIPLY | _ -> exit (-1) )
          | TDiv -> (
              match t with T_int | T_float -> DIVIDE | _ -> exit (-1) )
          | TMod -> ( match t with T_int | T_float -> MOD | _ -> exit (-1) )
          | TSub -> (
              match t with T_int | T_float -> SUBTRACT | _ -> exit (-1) )
          | TLessThan -> (
              match t with T_int | T_float -> CMPLT | _ -> exit (-1) )
          | TGreaterThan -> (
              match t with T_int | T_float -> CMPGT | _ -> exit (-1) )
          | TLessThanEq -> (
              match t with T_int | T_float -> CMPLE | _ -> exit (-1) )
          | TGreaterThanEq -> (
              match t with T_int | T_float -> CMPGE | _ -> exit (-1) )
          | TBEquals -> (
              match t with
              | T_int | T_float | T_string | T_char | T_bool | T_array _
              | T_obj _ ->
                  CMPEQ
              | _ -> exit (-1) )
          | TBNequals -> (
              match t with
              | T_int | T_float | T_string | T_char | T_bool | T_array _
              | T_obj _ ->
                  CMPNEQ
              | _ -> exit (-1) )
          | TBAnd -> ( match t with T_bool -> CMPAND | _ -> exit (-1) )
          | TBOr -> ( match t with T_bool -> CMPOR | _ -> exit (-1) ) )
          bytecode
  | TStructInit (_, name, _, fields) ->
      let f = get_struct name in
      append_bc (MAKE_OBJECT (List.length f)) bytecode
      @ List.fold_left
          (fun acc (field, _, expr) ->
            acc
            @ gen_expr_bytecode expr vars_table []
            @ [ SET_FIELD (get_struct_field name field) ])
          bytecode fields

and gen_arr_dec_bytecode dec bytecode =
  match dec with
  | TMultiDim (d, i) ->
      gen_arr_dec_bytecode d bytecode @ append_bc (MAKE_EMPTY_ARRAY i) bytecode
  | TSingleDim (t, i) ->
      append_bc (gen_default_val t) bytecode
      @ append_bc (MAKE_EMPTY_ARRAY i) bytecode

and gen_assign_bytecode (id, _, expression) vars_table bytecode =
  match id with
  | TVar (name, _) ->
      gen_expr_bytecode expression vars_table bytecode
      @ append_bc (STORE_VAR (vars_table#get name)) bytecode
  | TObjField (v, t, fields) -> (
      match t with
      | T_obj (name, _) ->
          append_bc (LOAD_VAR (vars_table#get v)) bytecode
          @ ( if List.length fields > 1 then
              gen_struct_field_getters
                (List.rev (List.tl (List.rev fields)))
                name bytecode
            else [] )
          @ gen_expr_bytecode expression vars_table bytecode
          @ List.rev (gen_struct_field_setters fields name bytecode)
      | _ ->
          Printf.eprintf "Unable to get field from non-object value";
          exit (-1) )

and gen_default_val t =
  match t with
  | T_any -> LOAD_CONST 0
  | T_int -> LOAD_CONST_I 0
  | T_float -> LOAD_CONST_F 0.0
  | T_bool -> LOAD_CONST_B false
  | T_char -> LOAD_CONST_C '0'
  | T_string -> LOAD_CONST_S ""
  | T_array _ -> MAKE_ARRAY 0
  | T_void | _ ->
      print_endline "Unable to assign void";
      exit (-1)

and gen_declare_bytecode declaration vars_table bytecode =
  match declaration with
  | name, t, expression -> (
      match expression with
      | None ->
          append_bc (gen_default_val t) bytecode
          @ append_bc (STORE_VAR (vars_table#insert name)) bytecode
      | Some e ->
          gen_expr_bytecode e vars_table bytecode
          @ append_bc (STORE_VAR (vars_table#insert name)) bytecode )

and gen_condition_bytecode condition jump_to vars_table bytecode =
  gen_expr_bytecode condition vars_table bytecode
  @ append_bc (JUMP_TRUE jump_to) bytecode

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
  match statements' with
  | Some s ->
      let stmts_else =
        List.fold_left
          (fun acc stmt -> acc @ gen_stmt_bytecode stmt vars_table [])
          bytecode s
      in
      let condition =
        gen_condition_bytecode condition
          (List.length stmts_if + 1)
          vars_table bytecode
      in
      condition @ stmts_if
      @ append_bc (JUMP (List.length stmts_else)) bytecode
      @ stmts_else
  | None ->
      let condition =
        gen_condition_bytecode condition (List.length stmts_if) vars_table
          bytecode
      in
      condition @ stmts_if

and gen_call_bytecode name params vars_table bytecode =
  List.fold_left
    (fun acc expr -> acc @ gen_expr_bytecode expr vars_table [])
    bytecode params
  @ append_bc
      (CALL (confirm_function name (List.length params), List.length params))
      bytecode

and gen_array_assign_getters exprs vars_table =
  List.fold_left
    (fun acc e ->
      acc @ gen_expr_bytecode e vars_table [] @ append_bc LOAD_FROM_ARRAY [])
    [] (List.rev exprs)

and gen_array_assign_setters exprs vars_table =
  List.fold_left
    (fun acc e ->
      acc @ gen_expr_bytecode e vars_table [] @ append_bc STORE_TO_ARRAY [])
    [] (List.rev exprs)

and gen_stmt_bytecode statement vars_table bytecode =
  match statement with
  | TDeclare (_, declaration) ->
      gen_declare_bytecode declaration vars_table bytecode
  | TAssign (_, assignment) ->
      gen_assign_bytecode assignment vars_table bytecode
  | TArrayAssign (_, ((v, exprs), expression)) ->
      ( if List.length exprs > 1 then
        gen_array_assign_getters
          (List.rev (List.tl (List.rev exprs)))
          vars_table
      else [] )
      @ gen_expr_bytecode expression vars_table bytecode
      @ gen_identifier_bytecode v vars_table bytecode
      @ List.rev (gen_array_assign_setters exprs vars_table)
      @ append_bc STORE_TO_ARRAY bytecode
      @ append_bc POP bytecode
  | TPrint (_, expression) ->
      gen_expr_bytecode expression vars_table bytecode
      @ append_bc PRINT bytecode
  | TPrintln (_, expression) ->
      gen_expr_bytecode expression vars_table bytecode
      @ append_bc PRINTLN bytecode
  | TIf (_, condition, _, statements, statements') ->
      gen_if_bytecode condition statements statements' vars_table bytecode
  | TWhile (_, condition, _, statements) ->
      gen_while_bytecode condition statements vars_table bytecode
  | TCall (_, (name, t, params)) -> (
      gen_call_bytecode name params vars_table bytecode
      @ match t with T_void -> [] | _ -> append_bc POP bytecode )
  | TReturn (_, expression, _) ->
      gen_expr_bytecode expression vars_table bytecode
      @ append_bc RETURN bytecode
  | TPass _ -> append_bc PASS bytecode

and gen_func_bytecode func vars_table bytecode =
  match func with
  | TFunc (_, name, _, params, block) ->
      List.iter (fun (_, param, _) -> ignore (vars_table#insert param)) params;
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

and gen_bytecode ((structs, ast) : t_structs * t_funcs) =
  List.iter insert_struct structs;
  gen_functions ast [] @ [ CALL (0, 0) ]
