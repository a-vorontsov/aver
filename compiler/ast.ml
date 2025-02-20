type loc = Lexing.position

let string_of_loc loc =
  Printf.sprintf "Line:%d Column:%d" loc.Lexing.pos_lnum
    (loc.Lexing.pos_cnum - loc.Lexing.pos_bol + 1)

type bop =
  | Add
  | Mult
  | Div
  | Sub
  | Mod
  | BAnd
  | BOr
  | BEquals
  | BNequals
  | GreaterThan
  | LessThan
  | GreaterThanEq
  | LessThanEq

type identifier = Var of string | ObjField of string * string list

type generic = Generic

and expr =
  | Input of loc
  | Null of loc
  | Num of loc * int
  | FNum of loc * float
  | Bool of loc * bool
  | Str of loc * string
  | Identifier of loc * identifier
  | Array of loc * expr list
  | ArrayAccess of loc * array_access
  | ArrayDec of loc * array_dec
  | Binop of loc * bop * expr * expr
  | AssignCall of loc * string * Types.type_expr option * expr list
  | StructInit of loc * string * Types.type_expr option * (string * expr) list

and array_dec =
  | SingleDim of Types.type_expr * int
  | MultiDim of array_dec * int

and array_access = identifier * expr list

and params = (loc * string * Types.type_expr) list

and stmt =
  | Declare of loc * string * Types.type_expr option * expr option
  | Assign of loc * identifier * expr
  | ArrayAssign of loc * array_access * expr
  | Print of loc * expr
  | Println of loc * expr
  | If of loc * expr * block * block option
  | While of loc * expr * block
  | Call of loc * string * Types.type_expr option * expr list
  | Return of loc * expr
  | Pass of loc

and block = stmt list

and func =
  | Func of loc * string * generic option * Types.type_expr * params * block

and funcs = func list

and struct_field = StructField of loc * string * Types.type_expr

and _struct = Struct of loc * string * generic option * struct_field list

and structs = _struct list

and prog = structs * funcs

let rec get_array_dec_base arr =
  match arr with
  | SingleDim (a, _) -> a
  | MultiDim (a, _) -> get_array_dec_base a
