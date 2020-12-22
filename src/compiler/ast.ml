type bop = Add | Mult | Div | Sub | Mod

and expr =
  | Input
  | Int of int
  | Var of string
  | Binop of bop * expr * expr
  | AssignCall of function_call

and booleanop = BEquals | BNequals | GreaterThan | LessThan

and condition = Bincond of booleanop * expr * expr

and params = string list

and assignment = string * expr

and function_call = string * expr list

and stmt =
  | Assign of assignment
  | Print of expr
  | If of condition * block * block
  | While of condition * block
  | Call of function_call
  | Return of expr
  | Pass

and block = stmt list

and func = Func of string * params * block

and funcs = func list

and prog = funcs
