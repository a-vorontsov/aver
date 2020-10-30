type bop = Add | Mult | Leq

type expr =
  | Var of string
  | Int of int
  | Bool of bool
  | Binop of bop * expr * expr
  | Let of string * expr
  | If of expr * expr * expr

type exprs = Exprs of expr list

type params = Params of string list

type func = Func of string * params * exprs

type funcs = func list

type prog = funcs
