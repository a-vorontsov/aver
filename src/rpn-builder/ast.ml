type bop = Add | Mult | Div | Sub

type expr =
  | Int of int
  | Var of string
  | Binop of bop * expr * expr

type assignment = string * expr

type stmt = Astmt of assignment | Aexpr of expr

type prog = stmt list
