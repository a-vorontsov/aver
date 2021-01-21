open Tast
open Types

let init_func_defs funcs =
  List.map
    (fun (Ast.Func (func_name, func_type, func_params, _)) ->
      (func_name, func_type, func_params))
    funcs

let init_env_with_params params =
  let env = Hashtbl.create 16 in
  List.iter
    (fun (param_name, param_type) ->
      if Hashtbl.mem env param_name then (
        print_endline "Function parameter declared twice";
        assert false )
      else Hashtbl.add env param_name param_type)
    params;
  env

let params_to_types params = List.map (fun (_, param_type) -> param_type) params

let var_exists_in_env var_name env = Hashtbl.mem env var_name

let get_var_in_env var_name env = Hashtbl.find env var_name

let get_func_type_from_defs func_name func_defs =
  let has_found =
    List.find_opt (fun (name, _, _) -> name = func_name) func_defs
  in
  match has_found with
  | Some (name, func_type, func_params) -> (name, func_type, func_params)
  | None ->
      print_endline "Unable to get type of undefined function";
      assert false

let rec type_program program =
  (* Pprint.pprint_prog program; *)
  type_funcs program (init_func_defs program)

and type_funcs funcs func_defs = List.map (fun f -> type_func f func_defs) funcs

and type_func (Ast.Func (func_name, func_type, func_params, func_body))
    func_defs =
  let t_body, return_type =
    type_block ~func_body ~func_defs ~block_type:T_void
      ~type_env:(init_env_with_params func_params)
  in
  if return_type = func_type then
    TFunc (func_name, func_type, func_params, t_body)
  else (
    print_endline "Function body return type doesn't match declared return type";
    assert false )

and type_block ~func_body ?(block_type = T_void) ~func_defs ~type_env =
  match func_body with
  | [] -> ([], T_void)
  | [ stmt ] -> (
      match type_stmt stmt func_defs type_env with
      | TReturn (t_expr, return_type), stmt_type ->
          ([ TReturn (t_expr, return_type) ], stmt_type)
      | t_stmt, stmt_type -> ([ t_stmt ], stmt_type) )
  | stmt :: stmts ->
      let t_stmt, stmt_type = type_stmt stmt func_defs type_env in
      let env' =
        match (t_stmt, stmt_type) with
        | t_stmt', _ -> (
            match t_stmt' with
            | TDeclare (var_name, var_type, _) ->
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
  | Ast.Declare (var_name, var_type, var_expr) ->
      if not (var_exists_in_env var_name type_env) then (
        match (var_type, var_expr) with
        | Some type_annotation, Some expr ->
            let typed_expr, expr_type = type_expr expr func_defs type_env in
            if type_annotation = expr_type then
              (TDeclare (var_name, type_annotation, Some typed_expr), T_void)
            else assert false
        | Some type_annotation, None ->
            (TDeclare (var_name, type_annotation, None), T_void)
        | None, Some expr ->
            let typed_expr, expr_type = type_expr expr func_defs type_env in
            (TDeclare (var_name, expr_type, Some typed_expr), T_void)
        | None, None ->
            print_endline
              "Variable declaration without explicit type or inferrable \
               expression not valid";
            assert false )
      else (
        print_endline "Variable declared twice";
        assert false )
  | Ast.Assign (var_name, var_expr) ->
      if var_exists_in_env var_name type_env then
        let typed_expr, expr_type = type_expr var_expr func_defs type_env in
        (TAssign (var_name, expr_type, typed_expr), T_void)
      else (
        print_endline "Unable to assign to undeclared variable";
        assert false )
  | Ast.Print expr ->
      let typed_expr, _ = type_expr expr func_defs type_env in
      (TPrint typed_expr, T_void)
  | Ast.Println expr ->
      let typed_expr, _ = type_expr expr func_defs type_env in
      (TPrintln typed_expr, T_void)
  | Ast.If (condition, block, block') ->
      let typed_condition, _ = type_bexpr condition func_defs type_env in
      let typed_block, block_type =
        type_block ~func_body:block ~block_type:T_void ~func_defs ~type_env
      in
      let typed_block', block_type' =
        type_block ~func_body:block' ~block_type:T_void ~func_defs ~type_env
      in
      if block_type = block_type' then
        ( TIf (typed_condition, block_type', typed_block, typed_block'),
          block_type' )
      else (
        print_endline "If/Then statement blocks are not of the same type";
        assert false )
  | Ast.While (condition, block) ->
      let typed_condition, _ = type_bexpr condition func_defs type_env in
      let typed_block, block_type =
        type_block ~func_body:block ~block_type:T_void ~func_defs ~type_env
      in
      (TWhile (typed_condition, block_type, typed_block), block_type)
  | Ast.Call (func_name, func_params) ->
      let typed_params, param_types =
        type_call_params func_params func_defs type_env
      in
      let _, func_type, func_params =
        get_func_type_from_defs func_name func_defs
      in
      if List.rev param_types = params_to_types func_params then
        (TCall (func_name, func_type, typed_params), T_void)
      else (
        print_endline "Function call parameter types do not match defined types";
        assert false )
  | Ast.Return expr ->
      let typed_expr, expr_type = type_expr expr func_defs type_env in
      (TReturn (typed_expr, expr_type), expr_type)
  | Ast.Pass -> (TPass, T_void)

and type_call_params params func_defs type_env =
  List.fold_left
    (fun (exprs, types) expr ->
      let typed_expr, expr_type = type_expr expr func_defs type_env in
      (typed_expr :: exprs, expr_type :: types))
    ([], []) params

and type_bexpr (Ast.Bincond (booleanop, expr, expr')) func_defs type_env =
  let typed_expr, expr_type = type_expr expr func_defs type_env in
  let typed_expr', expr_type' = type_expr expr' func_defs type_env in
  if expr_type = expr_type' then
    match booleanop with
    | BEquals ->
        (TBincond (TBEquals, expr_type, typed_expr, typed_expr'), T_bool)
    | BNequals ->
        (TBincond (TBNequals, expr_type, typed_expr, typed_expr'), T_bool)
    | GreaterThan ->
        (TBincond (TGreaterThan, expr_type, typed_expr, typed_expr'), T_bool)
    | GreaterThanEq ->
        (TBincond (TGreaterThanEq, expr_type, typed_expr, typed_expr'), T_bool)
    | LessThan ->
        (TBincond (TLessThan, expr_type, typed_expr, typed_expr'), T_bool)
    | LessThanEq ->
        (TBincond (TLessThanEq, expr_type, typed_expr, typed_expr'), T_bool)
  else (
    print_endline "LHS and RHS of boolean condition do not match";
    assert false )

and type_expr expr func_defs type_env =
  match expr with
  | Ast.Input -> (TInput, T_int)
  | Ast.Num i -> (TNum i, T_int)
  | Ast.FNum f -> (TFNum f, T_float)
  | Ast.Bool b -> (TBool b, T_bool)
  | Ast.Str s -> (TStr s, T_string)
  | Ast.Var s ->
      let var_type = get_var_in_env s type_env in
      (TVar (s, var_type), var_type)
  | Ast.Binop (b, e, e') ->
      let typed_expr, expr_type = type_expr e func_defs type_env in
      let typed_expr', expr_type' = type_expr e' func_defs type_env in
      if expr_type = expr_type' then (
        match expr_type with
        | T_int -> (
            match b with
            | Add ->
                (TBinop (expr_type, TAdd, typed_expr, typed_expr'), expr_type)
            | Mult ->
                (TBinop (expr_type, TMult, typed_expr, typed_expr'), expr_type)
            | Div ->
                (TBinop (expr_type, TDiv, typed_expr, typed_expr'), expr_type)
            | Sub ->
                (TBinop (expr_type, TSub, typed_expr, typed_expr'), expr_type)
            | Mod ->
                (TBinop (expr_type, TMod, typed_expr, typed_expr'), expr_type) )
        | T_float -> (
            match b with
            | Add ->
                (TBinop (expr_type, TAdd, typed_expr, typed_expr'), expr_type)
            | Mult ->
                (TBinop (expr_type, TMult, typed_expr, typed_expr'), expr_type)
            | Div ->
                (TBinop (expr_type, TDiv, typed_expr, typed_expr'), expr_type)
            | Sub ->
                (TBinop (expr_type, TSub, typed_expr, typed_expr'), expr_type)
            | Mod ->
                (TBinop (expr_type, TMod, typed_expr, typed_expr'), expr_type) )
        | T_string -> (
            match b with
            | Add ->
                (TBinop (expr_type, TAdd, typed_expr, typed_expr'), expr_type)
            | _ ->
                print_endline "Unable to perform binary operation on given type";
                assert false )
        | _ ->
            print_endline "Unable to perform binary operation on given type";
            assert false )
      else (
        print_endline "LHS and RHS of binary operation do not match";
        assert false )
  | Ast.AssignCall (func_name, func_params) ->
      let typed_params, param_types =
        type_call_params func_params func_defs type_env
      in
      let _, func_type, func_params =
        get_func_type_from_defs func_name func_defs
      in
      if param_types = params_to_types func_params then
        (TAssignCall (func_name, func_type, typed_params), func_type)
      else (
        print_endline "Function call parameter types do not match defined types";
        assert false )
