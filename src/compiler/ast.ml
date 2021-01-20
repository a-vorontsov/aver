type bop = Add | Mult | Div | Sub | Mod

and expr =
  | Input
  | Num of int
  | FNum of float
  | Bool of bool
  | Str of string
  | Var of string
  | Binop of bop * expr * expr
  | AssignCall of function_call

and booleanop =
  | BEquals
  | BNequals
  | GreaterThan
  | LessThan
  | GreaterThanEq
  | LessThanEq

and condition = Bincond of booleanop * expr * expr

and params = (string * Types.prim_type) list

and declaration = string * Types.prim_type option * expr option

and assignment = string * expr

and function_call = string * expr list

and stmt =
  | Declare of declaration
  | Assign of assignment
  | Print of expr
  | Println of expr
  | If of condition * block * block
  | While of condition * block
  | Call of function_call
  | Return of expr
  | Pass

and block = stmt list

and func = Func of string * Types.prim_type * params * block

and funcs = func list

and prog = funcs
