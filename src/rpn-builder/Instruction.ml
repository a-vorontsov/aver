open Printf

type instruction =
  | LOAD_CONST of int
  | LOAD_VAR of int
  | ADD
  | MULTIPLY
  | DIVIDE
  | SUBTRACT
  | MOD
  | STORE_VAR of int
  | PRINT
  | INPUT
  | CMPNEQ of int
  | CMPEQ of int
  | CMPLT of int
  | CMPGT of int
  | JUMP of int
  | PASS

type bytecode = instruction list

let as_string = function
  | LOAD_CONST i -> sprintf "0 %d" i
  | LOAD_VAR i -> sprintf "1 %d" i
  | ADD -> "2"
  | MULTIPLY -> "3"
  | DIVIDE -> "4"
  | SUBTRACT -> "5"
  | MOD -> "6"
  | STORE_VAR i -> sprintf "7 %d" i
  | PRINT -> "8"
  | INPUT -> "9"
  | CMPNEQ i -> sprintf "10 %d" i
  | CMPEQ i -> sprintf "11 %d" i
  | CMPLT i -> sprintf "12 %d" i
  | CMPGT i -> sprintf "13 %d" i
  | JUMP i -> sprintf "14 %d" i
  | PASS -> "15"
