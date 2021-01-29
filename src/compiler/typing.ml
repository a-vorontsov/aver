open Tast
open Types

let init_func_defs funcs =
  List.map
    (fun (Ast.Func (_, func_name, func_type, func_params, _)) ->
      (func_name, func_type, func_params))
    funcs

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

let get_var_in_env var_name env = Hashtbl.find env var_name

let get_func_type_from_defs func_name func_defs =
  let has_found =
    List.find_opt (fun (name, _, _) -> name = func_name) func_defs
  in
  match has_found with
  | Some (name, func_type, func_params) -> (name, func_type, func_params)
  | None ->
      Printf.eprintf "Unable to get type of undefined function\n";
      exit (-1)

let rec type_program program =
  (* Pprint.pprint_prog program; *)
  type_funcs program (init_func_defs program)

and type_funcs funcs func_defs = List.map (fun f -> type_func f func_defs) funcs

and type_func (Ast.Func (loc, func_name, func_type, func_params, func_body))
    func_defs =
  let t_body, return_type =
    type_block ~func_body ~func_defs ~block_type:T_void
      ~type_env:(init_env_with_params func_params)
  in
  if return_type = func_type then
    TFunc (loc, func_name, func_type, func_params, t_body)
  else (
    Printf.eprintf
      "%s\nFunction body return type doesn't match declared return type\n"
      (string_of_loc loc);
    exit (-1) )

and type_block ~func_body ?(block_type = T_void) ~func_defs ~type_env =
  match func_body with
  | [] -> ([], T_void)
  | [ stmt ] -> (
      match type_stmt stmt func_defs type_env with
      | TReturn (loc, t_expr, return_type), stmt_type ->
          ([ TReturn (loc, t_expr, return_type) ], stmt_type)
      | t_stmt, stmt_type -> ([ t_stmt ], stmt_type) )
  | stmt :: stmts ->
      let t_stmt, stmt_type = type_stmt stmt func_defs type_env in
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
        type_block ~func_body:stmts ~block_type ~func_defs ~type_env:env'
      in
      (t_stmt :: t_stmts, block_type')

and type_stmt stmt func_defs type_env =
  match stmt with
  | Ast.Declare (loc, (var_name, var_type, var_expr)) ->
      if not (var_exists_in_env var_name type_env) then (
        match (var_type, var_expr) with
        | Some type_annotation, Some expr ->
            let typed_expr, expr_type = type_expr expr func_defs type_env in
            if type_annotation = expr_type then
              ( TDeclare (loc, (var_name, type_annotation, Some typed_expr)),
                T_void )
            else exit (-1)
        | Some type_annotation, None ->
            (TDeclare (loc, (var_name, type_annotation, None)), T_void)
        | None, Some expr ->
            let typed_expr, expr_type = type_expr expr func_defs type_env in
            (TDeclare (loc, (var_name, expr_type, Some typed_expr)), T_void)
        | None, None ->
            Printf.eprintf
              "%s\n\
               Variable declaration without explicit type or inferrable \
               expression not valid\n"
              (string_of_loc loc);
            exit (-1) )
      else (
        Printf.eprintf "%s\nVariable declared twice\n" (string_of_loc loc);
        exit (-1) )
  | Ast.Assign (loc, (var_name, var_expr)) ->
      if var_exists_in_env var_name type_env then
        let typed_expr, expr_type = type_expr var_expr func_defs type_env in
        (TAssign (loc, (var_name, expr_type, typed_expr)), T_void)
      else (
        Printf.eprintf "%s\nUnable to assign to undeclared variable\n"
          (string_of_loc loc);
        exit (-1) )
  | Ast.ArrayAssign (loc, ((arr_name, idx_expr), expr)) ->
      let var_type =
        match get_var_in_env arr_name type_env with
        | T_array t -> t
        | _ ->
            Printf.eprintf "%s\nUnable to assign to undeclared variable\n"
              (string_of_loc loc);
            exit (-1)
      in
      let idx_typed_expr, idx_expr_type =
        type_expr idx_expr func_defs type_env
      in
      if idx_expr_type = T_int then
        let typed_expr, expr_type = type_expr expr func_defs type_env in
        if var_type = expr_type then
          (TArrayAssign (loc, ((arr_name, idx_typed_expr), typed_expr)), T_void)
        else (
          Printf.eprintf "%s\nUnable to assign to undeclared variable\n"
            (string_of_loc loc);
          exit (-1) )
      else (
        Printf.eprintf "%s\nUnable to assign to undeclared variable\n"
          (string_of_loc loc);
        exit (-1) )
  | Ast.Print (loc, expr) ->
      let typed_expr, _ = type_expr expr func_defs type_env in
      (TPrint (loc, typed_expr), T_void)
  | Ast.Println (loc, expr) ->
      let typed_expr, _ = type_expr expr func_defs type_env in
      (TPrintln (loc, typed_expr), T_void)
  | Ast.If (loc, condition, block, block') ->
      let typed_condition, _ = type_bexpr condition func_defs type_env in
      let typed_block, block_type =
        type_block ~func_body:block ~block_type:T_void ~func_defs ~type_env
      in
      let typed_block', block_type' =
        type_block ~func_body:block' ~block_type:T_void ~func_defs ~type_env
      in
      if block_type = block_type' then
        ( TIf (loc, typed_condition, block_type', typed_block, typed_block'),
          block_type' )
      else (
        Printf.eprintf "%s\nIf/Then statement blocks are not of the same type\n"
          (string_of_loc loc);
        exit (-1) )
  | Ast.While (loc, condition, block) ->
      let typed_condition, _ = type_bexpr condition func_defs type_env in
      let typed_block, block_type =
        type_block ~func_body:block ~block_type:T_void ~func_defs ~type_env
      in
      (TWhile (loc, typed_condition, block_type, typed_block), block_type)
  | Ast.Call (loc, (func_name, func_params)) ->
      let typed_params, param_types =
        type_call_params func_params func_defs type_env
      in
      let _, func_type, func_params =
        get_func_type_from_defs func_name func_defs
      in
      if List.rev param_types = params_to_types func_params then
        (TCall (loc, (func_name, func_type, typed_params)), T_void)
      else (
        Printf.eprintf
          "%s\nFunction call parameter types do not match defined types\n"
          (string_of_loc loc);
        exit (-1) )
  | Ast.Return (loc, expr) ->
      let typed_expr, expr_type = type_expr expr func_defs type_env in
      (TReturn (loc, typed_expr, expr_type), expr_type)
  | Ast.Pass loc -> (TPass loc, T_void)

and type_call_params params func_defs type_env =
  List.fold_left
    (fun (exprs, types) expr ->
      let typed_expr, expr_type = type_expr expr func_defs type_env in
      (typed_expr :: exprs, expr_type :: types))
    ([], []) params

and type_bexpr (Ast.Bincond (loc, booleanop, expr, expr')) func_defs type_env =
  let typed_expr, expr_type = type_expr expr func_defs type_env in
  let typed_expr', expr_type' = type_expr expr' func_defs type_env in
  if expr_type = expr_type' then
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
        (TBincond (loc, TLessThanEq, expr_type, typed_expr, typed_expr'), T_bool)
  else (
    Printf.eprintf "%s\nLHS and RHS of boolean condition do not match\n"
      (string_of_loc loc);
    exit (-1) )

and type_array arr func_defs type_env =
  let open List in
  let typed_exprs =
    map
      (fun e ->
        match type_expr e func_defs type_env with
        | typed_expr, expr_type -> (typed_expr, expr_type))
      arr
  in
  let e, t = split typed_exprs in
  let types =
    filter (fun t -> get_array_base_type t <> T_void) (sort_uniq compare t)
  in
  match length types with
  | 1 -> (e, types)
  | 0 -> (e, [ T_void ])
  | _ ->
      Printf.eprintf "Array contains values of more than one type\n";
      exit (-1)

and type_arr_dec dec =
  match dec with
  | Ast.SingleDim (t, s) -> (TSingleDim (t, s), T_array t)
  | Ast.MultiDim (a, s) ->
      let arr_dec, t = type_arr_dec a in
      (TMultiDim (arr_dec, s), T_array t)

and type_expr expr func_defs type_env =
  match expr with
  | Ast.Input loc -> (TInput loc, T_int)
  | Ast.Num (loc, i) -> (TNum (loc, i), T_int)
  | Ast.FNum (loc, f) -> (TFNum (loc, f), T_float)
  | Ast.Bool (loc, b) -> (TBool (loc, b), T_bool)
  | Ast.Str (loc, s) -> (TStr (loc, s), T_string)
  | Ast.Var (loc, s) ->
      let var_type = get_var_in_env s type_env in
      (TVar (loc, s, var_type), var_type)
  | Ast.Array (loc, exprs) ->
      let typed_exprs, arr_types = type_array exprs func_defs type_env in
      let arr_type = T_array (List.hd arr_types) in
      (TArray (loc, arr_type, typed_exprs), arr_type)
  | Ast.ArrayAccess (loc, (v, e)) ->
      let var_type = get_var_in_env v type_env in
      let typed_expr, expr_type = type_expr e func_defs type_env in
      if expr_type = T_int then (
        match var_type with
        | T_array t -> (TArrayAccess (loc, var_type, (v, typed_expr)), t)
        | _ ->
            Printf.eprintf "Invalid type\n";
            exit (-1) )
      else (
        Printf.eprintf "Arrays must be indexed using integers\n";
        exit (-1) )
  | Ast.ArrayDec (loc, a) ->
      let arr_dec, t = type_arr_dec a in
      (TArrayDec (loc, t, arr_dec), t)
  | Ast.Binop (loc, b, e, e') ->
      let typed_expr, expr_type = type_expr e func_defs type_env in
      let typed_expr', expr_type' = type_expr e' func_defs type_env in
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
                exit (-1) )
        | _ ->
            Printf.eprintf
              "%s\nUnable to perform binary operation on given type\n"
              (string_of_loc loc);
            exit (-1) )
      else (
        Printf.eprintf "%s\nLHS and RHS of binary operation do not match\n"
          (string_of_loc loc);
        exit (-1) )
  | Ast.AssignCall (loc, (func_name, func_params)) ->
      let typed_params, param_types =
        type_call_params func_params func_defs type_env
      in
      let _, func_type, func_params =
        get_func_type_from_defs func_name func_defs
      in
      if param_types = params_to_types func_params then
        (TAssignCall (loc, (func_name, func_type, typed_params)), func_type)
      else (
        Printf.eprintf
          "%s\nFunction call parameter types do not match defined types\n"
          (string_of_loc loc);
        exit (-1) )
