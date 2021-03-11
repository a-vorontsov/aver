open Tast
open Types

let init_env_with_params params =
  let env = Hashtbl.create 16 in
  List.iter
    (fun (loc, param_name, param_type) ->
      if Hashtbl.mem env param_name then (
        Printf.eprintf "%s\nFunction parameter declared twice\n"
          (string_of_loc loc);
        exit (-1) )
      else Hashtbl.add env param_name param_type)
    params;
  env

let params_to_types params =
  List.map (fun (_, _, param_type) -> param_type) params

let var_exists_in_env var_name env = Hashtbl.mem env var_name

let get_var_in_env var_name env =
  let has_found = Hashtbl.find_opt env var_name in
  match has_found with
  | Some x -> x
  | None ->
      Printf.eprintf "Unable to find variable %s\n" var_name;
      exit (-1)

let get_func_type_from_defs func_name func_defs =
  let has_found =
    List.find_opt
      (fun (Ast.Func (_, name, _, _, _, _)) -> name = func_name)
      func_defs
  in
  match has_found with
  | Some f -> f
  | None ->
      Printf.eprintf "Unable to get type of undefined function\n";
      exit (-1)

(* Check whether or not a struct has been declared *)
let struct_exists struct_name structs =
  let has_found =
    List.find_opt
      (fun (Ast.Struct (_, name, _, _)) -> name = struct_name)
      structs
  in
  match has_found with Some _ -> true | None -> false

(* Attempt to get a struct from list by name *)
let get_struct struct_name structs =
  let has_found =
    List.find_opt
      (fun (Ast.Struct (_, name, _, _)) -> name = struct_name)
      structs
  in
  match has_found with
  | Some s -> s
  | None ->
      Printf.eprintf "Unable to get undefined struct\n";
      exit (-1)

(* Check whether or not a field exists in a defined struct *)
let struct_field_exists (Ast.Struct (_, _, _, fields)) struct_field =
  let has_found =
    List.find_opt (fun (Ast.StructField (_, n, _)) -> n = struct_field) fields
  in
  match has_found with Some _ -> true | None -> false

(* Get field within a defined struct *)
let get_struct_field (Ast.Struct (_, _, _, f)) struct_field =
  let has_found =
    List.find_opt (fun (Ast.StructField (_, n, _)) -> n = struct_field) f
  in
  match has_found with
  | Some f -> f
  | None ->
      Printf.eprintf "Unable to get undefined struct field\n";
      exit (-1)

(* Type checking for the entire program *)
let rec type_program program =
  ( match Generics.type_generics_usage_program program with
  | Ok () -> ()
  | Error e ->
      Printf.eprintf "%s\n" e;
      exit (-1) );
  match program with
  | structs, funcs ->
      let struct_defs = structs in
      (type_structs structs struct_defs, type_funcs funcs funcs struct_defs)

(* Type checking for all declared structs *)
and type_structs structs struct_defs =
  List.map (fun s -> type_struct s struct_defs) structs

(* Type checking for structs *)
and type_struct (Ast.Struct (loc, struct_name, maybe_generic, struct_fields))
    struct_defs =
  let typed_fields =
    List.map (fun f -> type_struct_field f struct_defs) struct_fields
  in
  let struct_type =
    T_obj
      ( struct_name,
        match maybe_generic with
        | Some Ast.Generic -> Some T_generic
        | None -> None )
  in
  TStruct (loc, struct_name, struct_type, typed_fields)

(* Type checking for all defined fields in a struct *)
and type_struct_field (Ast.StructField (loc, field_name, field_type))
    struct_defs =
  let t =
    match field_type with
    | T_obj (f, _) ->
        if struct_exists f struct_defs then field_type
        else (
          Printf.eprintf "Written type is not defined\n";
          exit (-1) )
    | f -> f
  in
  TStructField (loc, field_name, t)

(* Apply type checking for all declared functions *)
and type_funcs funcs func_defs struct_defs =
  List.map (fun f -> type_func f func_defs struct_defs) funcs

(* Type checking for functions *)
and type_func (Ast.Func (loc, func_name, _, func_type, func_params, func_body))
    func_defs struct_defs =
  let t_body, return_type =
    type_block ~func_body ~func_defs ~block_type:T_void ~struct_defs
      ~type_env:(init_env_with_params func_params)
  in
  if return_type = func_type || return_type = T_any then
    TFunc (loc, func_name, func_type, func_params, t_body)
  else (
    Printf.eprintf
      "%s\nFunction body return type doesn't match declared return type\n"
      (string_of_loc loc);
    exit (-1) )

(* Type checking for block bodies *)
and type_block ~func_body ?(block_type = T_void) ~func_defs ~struct_defs
    ~type_env =
  match func_body with
  | [] -> ([], T_void)
  | [ stmt ] -> (
      match type_stmt stmt func_defs struct_defs type_env with
      | TReturn (loc, t_expr, return_type), stmt_type ->
          ([ TReturn (loc, t_expr, return_type) ], stmt_type)
      | t_stmt, stmt_type -> ([ t_stmt ], stmt_type) )
  | stmt :: stmts ->
      let t_stmt, stmt_type = type_stmt stmt func_defs struct_defs type_env in
      let env' =
        match (t_stmt, stmt_type) with
        | t_stmt', _ -> (
            match t_stmt' with
            | TDeclare (_, (var_name, var_type, _)) ->
                Hashtbl.add type_env var_name var_type;
                type_env
            | _ -> type_env )
      in
      let t_stmts, block_type' =
        type_block ~func_body:stmts ~block_type ~func_defs ~struct_defs
          ~type_env:env'
      in
      (t_stmt :: t_stmts, block_type')

(* Type checking for statements *)
and type_stmt stmt func_defs struct_defs type_env =
  match stmt with
  | Ast.Declare (loc, (var_name, var_type, var_expr)) ->
      if not (var_exists_in_env var_name type_env) then (
        match (var_type, var_expr) with
        | Some type_annotation, Some expr ->
            let typed_expr, expr_type =
              type_expr expr func_defs struct_defs type_env
            in
            if type_annotation = expr_type then
              ( TDeclare (loc, (var_name, type_annotation, Some typed_expr)),
                T_void )
            else (
              Printf.eprintf "%s\nAnnoted type does not match expression\n"
                (string_of_loc loc);
              Printf.eprintf "Expected: %s\nGot: %s\n"
                (type_to_string type_annotation)
                (type_to_string expr_type);
              exit (-1) )
        | Some type_annotation, None ->
            (TDeclare (loc, (var_name, type_annotation, None)), T_void)
        | None, Some expr ->
            let typed_expr, expr_type =
              type_expr expr func_defs struct_defs type_env
            in
            (TDeclare (loc, (var_name, expr_type, Some typed_expr)), T_void)
        | None, None ->
            Printf.eprintf
              "%s\n\
               Variable declaration without explicit type or inferrable \
               expression not valid\n"
              (string_of_loc loc);
            exit (-1) )
      else (
        Printf.eprintf "%s\nVariable, %s, declared twice\n" (string_of_loc loc)
          var_name;
        exit (-1) )
  | Ast.Assign (loc, (identifier, var_expr)) -> (
      let typed_id, id_type = type_identifier identifier struct_defs type_env in
      let typed_expr, expr_type =
        type_expr var_expr func_defs struct_defs type_env
      in
      match expr_type with
      | T_any -> (TAssign (loc, (typed_id, expr_type, typed_expr)), T_void)
      | _ ->
          if id_type = expr_type then
            (TAssign (loc, (typed_id, expr_type, typed_expr)), T_void)
          else (
            Printf.eprintf "%s\nAssignment type does not match variable type\n"
              (string_of_loc loc);
            Printf.eprintf "Expected: %s\nGot: %s\n" (type_to_string id_type)
              (type_to_string expr_type);
            exit (-1) ) )
  | Ast.ArrayAssign (loc, ((arr_name, idx_expr), expr)) ->
      let typed_id, id_type = type_identifier arr_name struct_defs type_env in
      let var_type =
        match id_type with
        | T_array t -> t
        | _ ->
            Printf.eprintf "%s\nUnable to assign to undeclared variable\n"
              (string_of_loc loc);
            exit (-1)
      in
      let idx_typed_expr, idx_expr_type =
        type_expr idx_expr func_defs struct_defs type_env
      in
      if idx_expr_type = T_int then
        let typed_expr, expr_type =
          type_expr expr func_defs struct_defs type_env
        in
        if var_type = expr_type then
          (TArrayAssign (loc, ((typed_id, idx_typed_expr), typed_expr)), T_void)
        else (
          Printf.eprintf "%s\nUnable to assign to undeclared variable\n"
            (string_of_loc loc);
          exit (-1) )
      else (
        Printf.eprintf "%s\nUnable to assign to undeclared variable\n"
          (string_of_loc loc);
        exit (-1) )
  | Ast.Print (loc, expr) ->
      let typed_expr, _ = type_expr expr func_defs struct_defs type_env in
      (TPrint (loc, typed_expr), T_void)
  | Ast.Println (loc, expr) ->
      let typed_expr, _ = type_expr expr func_defs struct_defs type_env in
      (TPrintln (loc, typed_expr), T_void)
  | Ast.If (loc, condition, block, block') -> (
      let typed_condition, _ =
        type_bexpr condition func_defs struct_defs type_env
      in
      let typed_block, block_type =
        type_block ~func_body:block ~block_type:T_void ~func_defs ~struct_defs
          ~type_env
      in
      let typed_block', block_type' =
        type_block ~func_body:block' ~block_type:T_void ~func_defs ~struct_defs
          ~type_env
      in
      match (block_type, block_type') with
      | T_any, T_any | T_any, _ | _, T_any ->
          ( TIf (loc, typed_condition, block_type', typed_block, typed_block'),
            block_type' )
      | _ ->
          if block_type = block_type' then
            ( TIf (loc, typed_condition, block_type', typed_block, typed_block'),
              block_type' )
          else (
            Printf.eprintf
              "%s\nIf/Then statement blocks are not of the same type\n"
              (string_of_loc loc);
            Printf.eprintf "If block: %s\nElse block: %s\n"
              (type_to_string block_type)
              (type_to_string block_type');
            exit (-1) ) )
  | Ast.While (loc, condition, block) ->
      let typed_condition, _ =
        type_bexpr condition func_defs struct_defs type_env
      in
      let typed_block, block_type =
        type_block ~func_body:block ~block_type:T_void ~func_defs ~struct_defs
          ~type_env
      in
      (TWhile (loc, typed_condition, block_type, typed_block), block_type)
  | Ast.Call (loc, (func_name, maybe_type_expr, func_params)) -> (
      let maybe_instantiated_function =
        Generics.instantiate_maybe_generic_function_def maybe_type_expr
          (get_func_type_from_defs func_name func_defs)
      in
      match maybe_instantiated_function with
      | Ok instantiated_function ->
          let typed_params, param_types =
            type_call_params func_params func_defs struct_defs type_env
          in
          let (Ast.Func (_, _, _, func_type, func_params, _)) =
            instantiated_function
          in
          if List.rev param_types = params_to_types func_params then
            (TCall (loc, (func_name, func_type, typed_params)), T_void)
          else (
            Printf.eprintf
              "%s\n\
               Function call parameter types do not match defined types\n\
               Expected: %s\n\
               Got: %s\n"
              (string_of_loc loc)
              (String.concat ", "
                 (List.map type_to_string (params_to_types func_params)))
              (String.concat ", "
                 (List.map type_to_string (List.rev param_types)));
            exit (-1) )
      | Error e ->
          Printf.eprintf "%s\n%s\n" (string_of_loc loc) e;
          exit (-1) )
  | Ast.Return (loc, expr) ->
      let typed_expr, expr_type =
        type_expr expr func_defs struct_defs type_env
      in
      (TReturn (loc, typed_expr, expr_type), expr_type)
  | Ast.Pass loc -> (TPass loc, T_void)

(* Type checking for function call parameters *)
and type_call_params params func_defs struct_defs type_env =
  List.fold_left
    (fun (exprs, types) expr ->
      let typed_expr, expr_type =
        type_expr expr func_defs struct_defs type_env
      in
      (typed_expr :: exprs, expr_type :: types))
    ([], []) params

(* Type checking for boolean conditional expressions *)
and type_bexpr (Ast.Bincond (loc, booleanop, expr, expr')) func_defs struct_defs
    type_env =
  let typed_expr, expr_type = type_expr expr func_defs struct_defs type_env in
  let typed_expr', expr_type' =
    type_expr expr' func_defs struct_defs type_env
  in
  match (expr_type, expr_type') with
  | T_any, T_any | T_any, _ | _, T_any -> (
      match booleanop with
      | BEquals ->
          (TBincond (loc, TBEquals, expr_type, typed_expr, typed_expr'), T_bool)
      | BNequals ->
          (TBincond (loc, TBNequals, expr_type, typed_expr, typed_expr'), T_bool)
      | GreaterThan ->
          ( TBincond (loc, TGreaterThan, expr_type, typed_expr, typed_expr'),
            T_bool )
      | GreaterThanEq ->
          ( TBincond (loc, TGreaterThanEq, expr_type, typed_expr, typed_expr'),
            T_bool )
      | LessThan ->
          (TBincond (loc, TLessThan, expr_type, typed_expr, typed_expr'), T_bool)
      | LessThanEq ->
          ( TBincond (loc, TLessThanEq, expr_type, typed_expr, typed_expr'),
            T_bool ) )
  | _ ->
      if expr_type = expr_type' then
        match booleanop with
        | BEquals ->
            ( TBincond (loc, TBEquals, expr_type, typed_expr, typed_expr'),
              T_bool )
        | BNequals ->
            ( TBincond (loc, TBNequals, expr_type, typed_expr, typed_expr'),
              T_bool )
        | GreaterThan ->
            ( TBincond (loc, TGreaterThan, expr_type, typed_expr, typed_expr'),
              T_bool )
        | GreaterThanEq ->
            ( TBincond (loc, TGreaterThanEq, expr_type, typed_expr, typed_expr'),
              T_bool )
        | LessThan ->
            ( TBincond (loc, TLessThan, expr_type, typed_expr, typed_expr'),
              T_bool )
        | LessThanEq ->
            ( TBincond (loc, TLessThanEq, expr_type, typed_expr, typed_expr'),
              T_bool )
      else (
        Printf.eprintf "%s\nLHS and RHS of boolean condition do not match\n"
          (string_of_loc loc);
        Printf.eprintf "LHS: %s\nRHS: %s\n" (type_to_string expr_type)
          (type_to_string expr_type');
        exit (-1) )

(* Type checking for values within arrays *)
and type_array arr func_defs struct_defs type_env =
  let open List in
  let typed_exprs =
    map
      (fun e ->
        match type_expr e func_defs struct_defs type_env with
        | typed_expr, expr_type -> (typed_expr, expr_type))
      arr
  in
  let e, t = split typed_exprs in
  let types =
    filter (fun t -> get_array_base_type t <> T_void) (sort_uniq compare t)
  in
  match length types with
  | 0 -> (e, [ T_void ])
  | 1 -> (e, types)
  | _ ->
      Printf.eprintf "Array contains values of more than one type\n";
      exit (-1)

(* Type checking for array declarations *)
and type_arr_dec dec =
  match dec with
  | Ast.SingleDim (t, s) -> (TSingleDim (t, s), T_array t)
  | Ast.MultiDim (a, s) ->
      let arr_dec, t = type_arr_dec a in
      (TMultiDim (arr_dec, s), T_array t)

(* Type checking for object fields *)
and type_fields fields struct_def =
  match fields with
  | [] ->
      Printf.eprintf "Unable to get type of non-existent field";
      exit 1
  | [ field ] ->
      let (Ast.StructField (_, _, field_type)) =
        get_struct_field struct_def field
      in
      [ (field, field_type) ]
  | field :: fields -> (
      let (Ast.StructField (_, _, field_type)) =
        get_struct_field struct_def field
      in
      match field_type with
      | T_obj _ -> [ (field, field_type) ] @ type_fields fields struct_def
      | _ ->
          let (Ast.Struct (_, struct_name, _, _)) = struct_def in
          Printf.eprintf "Unable to get field, %s, for non-object variable, %s"
            field struct_name;
          exit (-1) )

(* Type checking for variable identifiers *)
and type_identifier id struct_defs type_env =
  match id with
  | Ast.Var i ->
      let var_type = get_var_in_env i type_env in
      (TVar (i, var_type), var_type)
  | Ast.ObjField (i, f) -> (
      let var_type = get_var_in_env i type_env in
      match var_type with
      | T_obj (obj, _) ->
          let struct_def = get_struct obj struct_defs in
          let typed_fields = type_fields f struct_def in
          let _, field_type = List.hd (List.rev typed_fields) in
          (TObjField (i, var_type, typed_fields), field_type)
      | _ ->
          Printf.eprintf "Unable to get field(s) for non-object variable, %s" i;
          exit (-1) )

(* Type checking for struct field initialisation *)
and type_struct_field_init field struct_def func_defs struct_defs type_env =
  let field_name, field_expr = field in
  if struct_field_exists struct_def field_name then
    let (Ast.StructField (loc, _, field_type)) =
      get_struct_field struct_def field_name
    in
    let typed_field_expr, field_expr_type =
      type_expr field_expr func_defs struct_defs type_env
    in
    if field_type = field_expr_type then
      (field_name, field_expr_type, typed_field_expr)
    else (
      Printf.eprintf
        "%s\nField expression does not match defined struct field\n"
        (string_of_loc loc);
      Printf.eprintf "Expected: %s\nGot: %s\n"
        (type_to_string field_type)
        (type_to_string field_expr_type);
      exit (-1) )
  else (
    Printf.eprintf
      "Unable to initialise struct field, %s, that does not exist in struct\n"
      field_name;
    exit (-1) )

(* Type checking for single expressions *)
and type_expr expr func_defs struct_defs type_env =
  match expr with
  | Ast.Null loc -> (TNull loc, T_any)
  | Ast.Input loc -> (TInput loc, T_int)
  | Ast.Num (loc, i) -> (TNum (loc, i), T_int)
  | Ast.FNum (loc, f) -> (TFNum (loc, f), T_float)
  | Ast.Bool (loc, b) -> (TBool (loc, b), T_bool)
  | Ast.Str (loc, s) -> (TStr (loc, s), T_string)
  | Ast.Identifier (loc, i) ->
      let typed_id, id_type = type_identifier i struct_defs type_env in
      (TIdentifier (loc, typed_id), id_type)
  | Ast.Array (loc, exprs) ->
      let typed_exprs, arr_types =
        type_array exprs func_defs struct_defs type_env
      in
      let arr_type = T_array (List.hd arr_types) in
      (TArray (loc, arr_type, typed_exprs), arr_type)
  | Ast.ArrayAccess (loc, (v, e)) ->
      let typed_id, id_type = type_identifier v struct_defs type_env in
      let typed_expr, expr_type = type_expr e func_defs struct_defs type_env in
      if expr_type = T_int then (
        match id_type with
        | T_array t -> (TArrayAccess (loc, id_type, (typed_id, typed_expr)), t)
        | _ ->
            Printf.eprintf "Invalid type\n";
            Printf.eprintf "Expected: Array\nGot: %s\n" (type_to_string id_type);
            exit (-1) )
      else (
        Printf.eprintf "Arrays must be indexed using integers\n";
        Printf.eprintf "Expected: %s\nGot: %s\n" (type_to_string T_int)
          (type_to_string expr_type);
        exit (-1) )
  | Ast.ArrayDec (loc, a) ->
      let arr_dec, t = type_arr_dec a in
      (TArrayDec (loc, t, arr_dec), t)
  | Ast.Binop (loc, b, e, e') ->
      let typed_expr, expr_type = type_expr e func_defs struct_defs type_env in
      let typed_expr', expr_type' =
        type_expr e' func_defs struct_defs type_env
      in
      if expr_type = expr_type' then (
        match expr_type with
        | T_int ->
            ( TBinop
                ( loc,
                  expr_type,
                  ( match b with
                  | Add -> TAdd
                  | Mult -> TMult
                  | Div -> TDiv
                  | Sub -> TSub
                  | Mod -> TMod ),
                  typed_expr,
                  typed_expr' ),
              expr_type )
        | T_float ->
            ( TBinop
                ( loc,
                  expr_type,
                  ( match b with
                  | Add -> TAdd
                  | Mult -> TMult
                  | Div -> TDiv
                  | Sub -> TSub
                  | _ ->
                      Printf.eprintf
                        "%s\nUnable to perform binary operation on given type\n"
                        (string_of_loc loc);
                      Printf.eprintf "Got: %s\n" (type_to_string expr_type);
                      exit (-1) ),
                  typed_expr,
                  typed_expr' ),
              expr_type )
        | T_string -> (
            match b with
            | Add ->
                ( TBinop (loc, expr_type, TAdd, typed_expr, typed_expr'),
                  expr_type )
            | _ ->
                Printf.eprintf
                  "%s\nUnable to perform binary operation on given type\n"
                  (string_of_loc loc);
                Printf.eprintf "Got: %s\n" (type_to_string expr_type);
                exit (-1) )
        | _ ->
            Printf.eprintf
              "%s\nUnable to perform binary operation on given type\n"
              (string_of_loc loc);
            Printf.eprintf "Got: %s\n" (type_to_string expr_type);
            exit (-1) )
      else (
        Printf.eprintf "%s\nLHS and RHS of binary operation do not match\n"
          (string_of_loc loc);
        Printf.eprintf "LHS: %s\nRHS: %s\n" (type_to_string expr_type)
          (type_to_string expr_type');
        exit (-1) )
  | Ast.AssignCall (loc, (func_name, maybe_type_expr, func_params)) -> (
      let maybe_instantiated_function =
        Generics.instantiate_maybe_generic_function_def maybe_type_expr
          (get_func_type_from_defs func_name func_defs)
      in
      match maybe_instantiated_function with
      | Ok instantiated_function ->
          let typed_params, param_types =
            type_call_params func_params func_defs struct_defs type_env
          in
          let (Ast.Func (_, _, _, func_type, func_params, _)) =
            instantiated_function
          in
          if param_types = params_to_types func_params then
            (TAssignCall (loc, (func_name, func_type, typed_params)), func_type)
          else (
            Printf.eprintf
              "%s\n\
               Function call parameter types do not match defined types\n\
               Expected: %s\n\
               Got: %s" (string_of_loc loc)
              (String.concat ", "
                 (List.map type_to_string (params_to_types func_params)))
              (String.concat ", " (List.map type_to_string param_types));
            exit (-1) )
      | Error e ->
          Printf.eprintf "%s\n%s\n" (string_of_loc loc) e;
          exit (-1) )
  | Ast.StructInit (loc, struct_name, maybe_type_expr, struct_fields) -> (
      let maybe_instantiated_struct_def =
        Generics.instantiate_maybe_generic_struct_def maybe_type_expr
          (get_struct struct_name struct_defs)
      in
      match maybe_instantiated_struct_def with
      | Ok instantiated_struct_def ->
          let typed_struct_fields =
            List.map
              (fun f ->
                type_struct_field_init f instantiated_struct_def func_defs
                  struct_defs type_env)
              struct_fields
          in
          ( TStructInit
              ( loc,
                struct_name,
                T_obj (struct_name, maybe_type_expr),
                typed_struct_fields ),
            T_obj (struct_name, maybe_type_expr) )
      | Error e ->
          Printf.eprintf "%s\n%s\n" (string_of_loc loc) e;
          exit (-1) )
