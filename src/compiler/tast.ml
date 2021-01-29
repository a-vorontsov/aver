type loc = Lexing.position

let string_of_loc loc =
  Printf.sprintf "Line:%d Position:%d" loc.Lexing.pos_lnum
    (loc.Lexing.pos_cnum - loc.Lexing.pos_bol + 1)

type t_bop = TAdd | TMult | TDiv | TSub | TMod

and t_expr =
  | TInput of loc
  | TNum of loc * int
  | TFNum of loc * float
  | TBool of loc * bool
  | TStr of loc * string
  | TVar of loc * string * Types.type_expr
  | TArray of loc * Types.type_expr * t_expr list
  | TArrayAccess of loc * Types.type_expr * t_array_access
  | TArrayDec of loc * Types.type_expr * t_array_dec
  | TBinop of loc * Types.type_expr * t_bop * t_expr * t_expr
  | TAssignCall of loc * t_function_call

and t_array_dec =
  | TSingleDim of Types.type_expr * int
  | TMultiDim of t_array_dec * int

and t_array_access = string * t_expr

and t_booleanop =
  | TBEquals
  | TBNequals
  | TGreaterThan
  | TLessThan
  | TGreaterThanEq
  | TLessThanEq

and t_condition =
  | TBincond of loc * t_booleanop * Types.type_expr * t_expr * t_expr

and t_params = (loc * string * Types.type_expr) list

and t_declaration = string * Types.type_expr * t_expr option

and t_assignment = string * Types.type_expr * t_expr

and t_array_assignment = t_array_access * t_expr

and t_function_call = string * Types.type_expr * t_expr list

and t_stmt =
  | TDeclare of loc * t_declaration
  | TAssign of loc * t_assignment
  | TArrayAssign of loc * t_array_assignment
  | TPrint of loc * t_expr
  | TPrintln of loc * t_expr
  | TIf of loc * t_condition * Types.type_expr * t_block * t_block
  | TWhile of loc * t_condition * Types.type_expr * t_block
  | TCall of loc * t_function_call
  | TReturn of loc * t_expr * Types.type_expr
  | TPass of loc

and t_block = t_stmt list

and t_func = TFunc of loc * string * Types.type_expr * t_params * t_block

and t_funcs = t_func list

and t_prog = t_funcs
