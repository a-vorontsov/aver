type bop = Add | Mult | Div | Sub | Mod

type expr = Input | Int of int | Var of string | Binop of bop * expr * expr

type booleanop = BEquals | BNequals | GreaterThan | LessThan

type condition = Bincond of booleanop * expr * expr

type assignment = string * expr

type stmt =
  | Assign of assignment
  | Print of expr
  | If of condition * block * block
  | While of condition * block
  | Pass

and block = stmt list

type prog = block
