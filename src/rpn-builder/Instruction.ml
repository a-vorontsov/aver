open Printf

type instruction =
  | LOAD_CONST of int
  | LOAD_VAR of int
  | ADD
  | MULTIPLY
  | DIVIDE
  | SUBTRACT
  | STORE_VAR of int
  | PRINT
  | INPUT
  | CMPNEQ of int
  | CMPEQ of int
  | CMPLT of int
  | CMPGT of int
  | JUMP of int

type bytecode = instruction list

let as_string = function
  | LOAD_CONST i -> sprintf "0 %d" i
  | LOAD_VAR i -> sprintf "1 %d" i
  | ADD -> "2"
  | MULTIPLY -> "3"
  | DIVIDE -> "4"
  | SUBTRACT -> "5"
  | STORE_VAR i -> sprintf "6 %d" i
  | PRINT -> "7"
  | INPUT -> "8"
  | CMPNEQ i -> sprintf "9 %d" i
  | CMPEQ i -> sprintf "10 %d" i
  | CMPLT i -> sprintf "11 %d" i
  | CMPGT i -> sprintf "12 %d" i
  | JUMP i -> sprintf "13 %d" i
