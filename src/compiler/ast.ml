type bop = Add | Mult | Div | Sub | Mod

and prim_type = T_int | T_bool | T_char | T_string | T_void

and expr =
  | Input
  | Num of int
  | Var of string
  | Binop of bop * expr * expr
  | AssignCall of function_call

and booleanop = BEquals | BNequals | GreaterThan | LessThan

and condition = Bincond of booleanop * expr * expr

and params = (string * prim_type) list

and declaration = string * prim_type * expr option

and assignment = string * expr

and function_call = string * expr list

and stmt =
  | Declare of declaration
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
