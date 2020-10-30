type bop = Add | Mult | Div | Sub

type expr =
  | Int of int
  | Binop of bop * expr * expr
