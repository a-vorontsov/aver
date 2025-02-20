open Types
open Ast

let rec type_generics_usage_program (structs, funcs) =
  let r = type_generics_usage_structs structs in
  if Result.is_ok r then type_generics_usage_funcs funcs else r

and type_generics_usage_funcs funcs =
  let results = List.map (fun f -> type_generics_usage_func_def f) funcs in
  if List.for_all (fun r -> Result.is_ok r) results then Ok ()
  else Error "Use of generic type but not in generic function"

and type_generics_usage_structs structs =
  let results = List.map (fun s -> type_generics_usage_struct_def s) structs in
  if List.for_all (fun r -> Result.is_ok r) results then Ok ()
  else Error "Use of generic type but not in generic struct"

and type_generics_usage_struct_def (Struct (_, _, maybe_generic, struct_fields))
    =
  let results =
    List.map
      (fun (StructField (_, _, t)) -> type_generics_usage_type t maybe_generic)
      struct_fields
  in
  if List.for_all (fun r -> Result.is_ok r) results then Ok ()
  else Error "Use of generic type but not in generic struct"

and type_generics_usage_func_def
    (Func (_, _, maybe_generic, type_sig, params, block)) =
  let r = type_generics_usage_type_sig type_sig params maybe_generic in
  if Result.is_ok r then type_generics_usage_block block maybe_generic else r

and type_generics_usage_type_sig type_sig params maybe_in_generic_structure =
  let r = type_generics_usage_type type_sig maybe_in_generic_structure in
  if Result.is_ok r then
    let results =
      List.map
        (fun (_, _, t) -> type_generics_usage_type t maybe_in_generic_structure)
        params
    in
    if List.for_all (fun r -> Result.is_ok r) results then Ok ()
    else Error "Use of generic type but not in generic function"
  else r

and type_generics_usage_block block maybe_in_generic_structure =
  match maybe_in_generic_structure with
  | Some Generic -> Ok ()
  | None ->
      let results =
        List.map
          (fun s -> type_generics_usage_stmt s maybe_in_generic_structure)
          block
      in
      if List.for_all (fun r -> Result.is_ok r) results then Ok ()
      else Error "Use of generic type but not in generic block"

and type_generics_usage_stmt stmt maybe_in_generic_structure =
  match maybe_in_generic_structure with
  | Some Generic -> Ok ()
  | None -> (
      match stmt with
      | Declare (_, _, maybe_type, maybe_expr) ->
          let r =
            type_generics_usage_maybe_type maybe_type maybe_in_generic_structure
          in
          if Result.is_ok r then
            match maybe_expr with
            | Some expr ->
                type_generics_usage_expr expr maybe_in_generic_structure
            | None -> Ok ()
          else r
      | Assign (_, _, expr) ->
          type_generics_usage_expr expr maybe_in_generic_structure
      | ArrayAssign (_, _, expr) ->
          type_generics_usage_expr expr maybe_in_generic_structure
      | Print (_, expr) ->
          type_generics_usage_expr expr maybe_in_generic_structure
      | Println (_, expr) ->
          type_generics_usage_expr expr maybe_in_generic_structure
      | If (_, cond, block, block') ->
          let r = type_generics_usage_expr cond maybe_in_generic_structure in
          if Result.is_ok r then
            let r' =
              type_generics_usage_block block maybe_in_generic_structure
            in
            if Result.is_ok r' then
              match block' with
              | Some block ->
                  type_generics_usage_block block maybe_in_generic_structure
              | None -> r'
            else r'
          else r
      | While (_, cond, block) ->
          let r = type_generics_usage_expr cond maybe_in_generic_structure in
          if Result.is_ok r then
            type_generics_usage_block block maybe_in_generic_structure
          else r
      | Call (_, _, maybe_type_param, exprs) ->
          let r =
            type_generics_usage_maybe_type maybe_type_param
              maybe_in_generic_structure
          in
          if Result.is_ok r then
            let results =
              List.map
                (fun e -> type_generics_usage_expr e maybe_in_generic_structure)
                exprs
            in
            if List.for_all (fun r -> Result.is_ok r) results then Ok ()
            else Error "Use of generic type but not in generic block"
          else r
      | Return (_, expr) ->
          type_generics_usage_expr expr maybe_in_generic_structure
      | Pass _ -> Ok () )

and type_generics_usage_expr expr maybe_in_generic_structure =
  match maybe_in_generic_structure with
  | Some Generic -> Ok ()
  | None -> (
      match expr with
      | Input _ | Null _ | Num _ | FNum _ | Bool _ | Str _ | Identifier _ ->
          Ok ()
      | Array (_, exprs) ->
          let results =
            List.map
              (fun e -> type_generics_usage_expr e maybe_in_generic_structure)
              exprs
          in
          if List.for_all (fun r -> Result.is_ok r) results then Ok ()
          else Error "Use of generic type but not in generic block"
      | ArrayAccess (_, access) ->
          type_generics_usage_array_access access maybe_in_generic_structure
      | ArrayDec (_, a) ->
          type_generics_usage_type (get_array_dec_base a)
            maybe_in_generic_structure
      | Binop (_, _, e, e') -> (
          let r = type_generics_usage_expr e maybe_in_generic_structure in
          let r' = type_generics_usage_expr e' maybe_in_generic_structure in
          match (r, r') with
          | Ok (), Ok () -> r
          | Error _, _ -> r
          | _, Error _ -> r' )
      | AssignCall (_, _, maybe_type_param, exprs) ->
          let r =
            type_generics_usage_maybe_type maybe_type_param
              maybe_in_generic_structure
          in
          if Result.is_ok r then
            let results =
              List.map
                (fun e -> type_generics_usage_expr e maybe_in_generic_structure)
                exprs
            in
            if List.for_all (fun r -> Result.is_ok r) results then Ok ()
            else Error "Use of generic type but not in generic block"
          else r
      | StructInit (_, _, maybe_type_param, args) ->
          let r =
            type_generics_usage_maybe_type maybe_type_param
              maybe_in_generic_structure
          in
          if Result.is_ok r then
            let open List in
            let results =
              map
                (fun e -> type_generics_usage_expr e maybe_in_generic_structure)
                (map (fun (_, e) -> e) args)
            in
            if List.for_all (fun r -> Result.is_ok r) results then Ok ()
            else Error "Use of generic type but not in generic block"
          else r )

and type_generics_usage_array_access (_, exprs) maybe_in_generic_structure =
  match maybe_in_generic_structure with
  | Some Generic -> Ok ()
  | None ->
      let results =
        List.map
          (fun e -> type_generics_usage_expr e maybe_in_generic_structure)
          exprs
      in
      if List.for_all (fun r -> Result.is_ok r) results then Ok ()
      else Error "Use of generic type in array access but not in generic block"

and type_generics_usage_maybe_type maybe_type_expr maybe_in_generic_structure =
  match maybe_type_expr with
  | Some type_expr ->
      type_generics_usage_type type_expr maybe_in_generic_structure
  | None -> Ok ()

and type_generics_usage_type type_expr maybe_in_generic_structure =
  match maybe_in_generic_structure with
  | Some Generic -> Ok ()
  | None -> (
      match type_expr with
      | T_generic -> Error "Use of generic type but not in generic block"
      | T_obj (_, t) -> (
          match t with
          | Some t' -> type_generics_usage_type t' maybe_in_generic_structure
          | None -> Ok () )
      | T_array a -> type_generics_usage_type a maybe_in_generic_structure
      | _ -> Ok () )

let rec instantiate_maybe_generic_struct_def maybe_type_param
    (Struct (loc, struct_name, maybe_generic, struct_fields) as struct_def) =
  match (maybe_generic, maybe_type_param) with
  | None, None -> Ok struct_def
  | None, Some type_param ->
      Error
        (Printf.sprintf
           "non-generic struct, %s, is being instantiated with type parameter, \
            %s"
           struct_name
           (type_to_string type_param))
  | Some Generic, None ->
      Error
        (Printf.sprintf
           "generic struct, %s, needs to be instantiated with a type parameter"
           struct_name)
  | Some Generic, Some type_param ->
      Ok
        (Struct
           ( loc,
             struct_name,
             maybe_generic,
             instantiate_maybe_generic_struct_fields type_param struct_fields ))

and instantiate_maybe_generic_struct_fields type_param struct_fields =
  List.map (instantiate_maybe_generic_struct_field type_param) struct_fields

and instantiate_maybe_generic_struct_field type_param
    (StructField (loc, field_name, type_expr)) =
  StructField
    (loc, field_name, instantiate_maybe_generic_type type_param type_expr)

and instantiate_maybe_generic_function_def maybe_type_param
    (Func (loc, func_name, maybe_generic, type_sig, params, block) as func_def)
    =
  match (maybe_generic, maybe_type_param) with
  | None, None -> Ok func_def
  | None, Some type_param ->
      Error
        (Printf.sprintf
           "non-generic function, %s, is being instantiated with type \
            parameter, %s"
           func_name
           (type_to_string type_param))
  | Some Generic, None ->
      Error
        (Printf.sprintf
           "generic function, %s, needs to be instantiated with a type \
            parameter"
           func_name)
  | Some Generic, Some type_param ->
      Ok
        (Func
           ( loc,
             func_name,
             maybe_generic,
             instantiate_maybe_generic_type type_param type_sig,
             instantiate_maybe_generic_function_params type_param params,
             block ))

and instantiate_maybe_generic_function_params type_param params =
  List.map (instantiate_maybe_generic_function_param type_param) params

and instantiate_maybe_generic_function_param type_param
    (loc, param_name, type_expr) =
  (loc, param_name, instantiate_maybe_generic_type type_param type_expr)

and instantiate_maybe_generic_type type_param type_expr =
  match type_expr with
  | T_generic -> type_param
  | T_obj (struct_name, maybe_param_type) ->
      let updated_param =
        match maybe_param_type with
        | Some maybe_in_generic_struct_param_type ->
            Some
              (instantiate_maybe_generic_type type_param
                 maybe_in_generic_struct_param_type)
        | None -> None
      in
      T_obj (struct_name, updated_param)
  | T_array t -> T_array (instantiate_maybe_generic_type type_param t)
  | _ -> type_expr
