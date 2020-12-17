type bop = Add | Mult | Div | Sub | Mod

type expr = Input | Int of int | Var of string | Binop of bop * expr * expr

type booleanop = BEquals | BNequals | GreaterThan | LessThan

type condition = Bincond of booleanop * expr * expr

type params = Params of string list

type assignment = string * expr

type stmt =
  | Assign of assignment
  | Print of expr
  | If of condition * block * block
  | While of condition * block
  | Call of string * expr list
  | Pass

and block = stmt list

type func = Func of string * params * block

type funcs = func list

type prog = funcs
