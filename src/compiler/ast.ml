type loc = Lexing.position

let string_of_loc loc =
  Printf.sprintf "Line:%d Column:%d" loc.Lexing.pos_lnum
    (loc.Lexing.pos_cnum - loc.Lexing.pos_bol + 1)

type bop = Add | Mult | Div | Sub | Mod

and expr =
  | Input of loc
  | Num of loc * int
  | FNum of loc * float
  | Bool of loc * bool
  | Str of loc * string
  | Var of loc * string
  | Array of loc * expr list
  | ArrayAccess of loc * array_access
  | ArrayDec of loc * array_dec
  | Binop of loc * bop * expr * expr
  | AssignCall of loc * function_call

and array_dec =
  | SingleDim of Types.type_expr * int
  | MultiDim of array_dec * int

and array_access = string * expr

and booleanop =
  | BEquals
  | BNequals
  | GreaterThan
  | LessThan
  | GreaterThanEq
  | LessThanEq

and condition = Bincond of loc * booleanop * expr * expr

and params = (loc * string * Types.type_expr) list

and declaration = string * Types.type_expr option * expr option

and assignment = string * expr

and array_assignment = array_access * expr

and function_call = string * expr list

and stmt =
  | Declare of loc * declaration
  | Assign of loc * assignment
  | ArrayAssign of loc * array_assignment
  | Print of loc * expr
  | Println of loc * expr
  | If of loc * condition * block * block
  | While of loc * condition * block
  | Call of loc * function_call
  | Return of loc * expr
  | Pass of loc

and block = stmt list

and func = Func of loc * string * Types.type_expr * params * block

and funcs = func list

and prog = funcs
