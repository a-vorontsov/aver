open Ast
open Printf
open Types

let indent_space = "    "

let pprint_op op =
  match op with
  | Add -> "+"
  | Mult -> "*"
  | Div -> "/"
  | Sub -> "-"
  | Mod -> "%"

let pprint_bop op =
  match op with
  | BEquals -> "=="
  | BNequals -> "!="
  | GreaterThan -> ">"
  | LessThan -> "<"
  | GreaterThanEq -> ">="
  | LessThanEq -> "<="

let rec pprint_expr ~indent expr =
  let print_expr = printf "%sExpr: %s\n" indent in
  let new_indent = indent_space ^ indent in
  match expr with
  | Input -> print_expr "Input"
  | Num i -> print_expr (sprintf "Int %d" i)
  | FNum f -> print_expr (sprintf "Float %f" f)
  | Bool b -> print_expr (sprintf "Bool %b" b)
  | Str s -> print_expr (sprintf "String %s" s)
  | Var v -> print_expr (sprintf "Var %s" v)
  | Binop (b, e, e') ->
      print_expr (sprintf "Op %s" (pprint_op b));
      pprint_expr ~indent:new_indent e;
      pprint_expr ~indent:new_indent e'
  | AssignCall (function_name, args) ->
      print_expr (sprintf "Call %s" function_name);
      List.iter (pprint_expr ~indent) args

and pprint_condition ~indent (Bincond (op, expr, expr')) =
  let print_condition = printf "%sCondition: %s\n" indent in
  let new_indent = indent_space ^ indent in
  print_condition (sprintf "Op %s" (pprint_bop op));
  pprint_expr ~indent:new_indent expr;
  pprint_expr ~indent:new_indent expr'

and pprint_stmt ~indent stmt =
  let print_stmt = printf "%sStmt: %s\n" indent in
  let new_indent = indent_space ^ indent in
  match stmt with
  | Declare (name, dec_type, expr) -> (
      print_stmt
        (sprintf "Declare %s%s" name
           ( match dec_type with
           | Some t -> sprintf " of type %s" (type_to_string t)
           | None -> "" ));
      match expr with Some e -> pprint_expr ~indent:new_indent e | None -> () )
  | Assign (name, expr) ->
      print_stmt (sprintf "Assign %s" name);
      pprint_expr ~indent:new_indent expr
  | Print expr ->
      print_stmt "Print";
      pprint_expr ~indent:new_indent expr
  | Println expr ->
      print_stmt "Println";
      pprint_expr ~indent:new_indent expr
  | If (condition, block, block') ->
      print_stmt "If";
      pprint_condition ~indent:new_indent condition;
      pprint_block ~indent:new_indent ~block_name:"If" block;
      pprint_block ~indent:new_indent ~block_name:"Else" block'
  | While (condition, block) ->
      print_stmt "While";
      pprint_condition ~indent:new_indent condition;
      pprint_block ~indent:new_indent ~block_name:"While" block
  | Call (function_name, args) ->
      print_stmt (sprintf "Call %s" function_name);
      List.iter (pprint_expr ~indent) args
  | Return expr ->
      print_stmt "Return";
      pprint_expr ~indent:new_indent expr
  | Pass -> print_stmt "Pass"

and pprint_block ~indent ~block_name block =
  let new_indent = indent_space ^ indent in
  printf "%s%s block\n" indent block_name;
  List.iter (pprint_stmt ~indent:new_indent) block

let pprint_param ~indent (param_name, param_type) =
  printf "%s Param: %s of type %s\n" indent param_name
    (type_to_string param_type)

let pprint_params ~indent = function
  | [] -> printf "%s\n" indent
  | params -> List.iter (pprint_param ~indent) params

let pprint_function ~indent
    (Func (func_name, func_type, func_params, func_body)) =
  let new_indent = indent_space ^ indent in
  printf "%s Function: %s\n" indent func_name;
  printf "%s Returns: %s\n" indent (type_to_string func_type);
  pprint_params ~indent:new_indent func_params;
  pprint_block ~indent:new_indent ~block_name:"Body" func_body

let pprint_prog prog =
  let indent = "└──" in
  List.iter (fun f -> ignore (pprint_function ~indent f)) prog
