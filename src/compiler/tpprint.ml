open Tast
open Printf
open Types

let indent_space = "    "

let pprint_op op =
  match op with
  | TAdd -> "+"
  | TMult -> "*"
  | TDiv -> "/"
  | TSub -> "-"
  | TMod -> "%"

let pprint_bop op =
  match op with
  | TBEquals -> "=="
  | TBNequals -> "!="
  | TGreaterThan -> ">"
  | TLessThan -> "<"
  | TGreaterThanEq -> ">="
  | TLessThanEq -> "<="

let rec pprint_expr ~indent expr =
  let print_expr = printf "%sExpr: %s\n" indent in
  let new_indent = indent_space ^ indent in
  match expr with
  | TInput -> print_expr "Input"
  | TNum i -> print_expr (sprintf "Int %d" i)
  | TFNum f -> print_expr (sprintf "Float %f" f)
  | TBool b -> print_expr (sprintf "Bool %b" b)
  | TStr s -> print_expr (sprintf "String %s" s)
  | TVar (v, _) -> print_expr (sprintf "Var %s" v)
  | TBinop (_, b, e, e') ->
      print_expr (sprintf "Op %s" (pprint_op b));
      pprint_expr ~indent:new_indent e;
      pprint_expr ~indent:new_indent e'
  | TAssignCall (function_name, _, args) ->
      print_expr (sprintf "Call %s" function_name);
      List.iter (pprint_expr ~indent) args

and pprint_condition ~indent (TBincond (op, _, expr, expr')) =
  let print_condition = printf "%sCondition: %s\n" indent in
  let new_indent = indent_space ^ indent in
  print_condition (sprintf "Op %s" (pprint_bop op));
  pprint_expr ~indent:new_indent expr;
  pprint_expr ~indent:new_indent expr'

and pprint_stmt ~indent stmt =
  let print_stmt = printf "%sStmt: %s\n" indent in
  let new_indent = indent_space ^ indent in
  match stmt with
  | TDeclare (name, dec_type, expr) -> (
      print_stmt
        (sprintf "Declare %s%s" name
           (sprintf " of type %s" (type_to_string dec_type)));
      match expr with Some e -> pprint_expr ~indent:new_indent e | None -> () )
  | TAssign (name, _, expr) ->
      print_stmt (sprintf "Assign %s" name);
      pprint_expr ~indent:new_indent expr
  | TPrint expr ->
      print_stmt "Print";
      pprint_expr ~indent:new_indent expr
  | TPrintln expr ->
      print_stmt "Println";
      pprint_expr ~indent:new_indent expr
  | TIf (condition, _, block, block') ->
      print_stmt "If";
      pprint_condition ~indent:new_indent condition;
      pprint_block ~indent:new_indent ~block_name:"If" block;
      pprint_block ~indent:new_indent ~block_name:"Else" block'
  | TWhile (condition, _, block) ->
      print_stmt "While";
      pprint_condition ~indent:new_indent condition;
      pprint_block ~indent:new_indent ~block_name:"While" block
  | TCall (function_name, _, args) ->
      print_stmt (sprintf "Call %s" function_name);
      List.iter (pprint_expr ~indent) args
  | TReturn (expr, _) ->
      print_stmt "Return";
      pprint_expr ~indent:new_indent expr
  | TPass -> print_stmt "Pass"

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
    (TFunc (func_name, func_type, func_params, func_body)) =
  let new_indent = indent_space ^ indent in
  printf "%s Function: %s\n" indent func_name;
  printf "%s Returns: %s\n" indent (type_to_string func_type);
  pprint_params ~indent:new_indent func_params;
  pprint_block ~indent:new_indent ~block_name:"Body" func_body

let pprint_prog prog =
  let indent = "└──" in
  List.iter (fun f -> ignore (pprint_function ~indent f)) prog
