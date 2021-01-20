type t_bop = TAdd | TMult | TDiv | TSub | TMod

and t_expr =
  | TInput
  | TNum of int
  | TFNum of float
  | TBool of bool
  | TStr of string
  | TVar of string * Types.prim_type
  | TBinop of Types.prim_type * t_bop * t_expr * t_expr
  | TAssignCall of t_function_call

and t_booleanop =
  | TBEquals
  | TBNequals
  | TGreaterThan
  | TLessThan
  | TGreaterThanEq
  | TLessThanEq

and t_condition = TBincond of t_booleanop * Types.prim_type * t_expr * t_expr

and t_params = (string * Types.prim_type) list

and t_declaration = string * Types.prim_type * t_expr option

and t_assignment = string * Types.prim_type * t_expr

and t_function_call = string * Types.prim_type * t_expr list

and t_stmt =
  | TDeclare of t_declaration
  | TAssign of t_assignment
  | TPrint of t_expr
  | TPrintln of t_expr
  | TIf of t_condition * Types.prim_type * t_block * t_block
  | TWhile of t_condition * Types.prim_type * t_block
  | TCall of t_function_call
  | TReturn of t_expr * Types.prim_type
  | TPass

and t_block = t_stmt list

and t_func = TFunc of string * Types.prim_type * t_params * t_block

and t_funcs = t_func list

and t_prog = t_funcs
