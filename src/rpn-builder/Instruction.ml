open Printf

type instruction =
  | LOAD_CONST of int
  | LOAD_VAR of string
  | ADD
  | MULTIPLY
  | DIVIDE
  | SUBTRACT
  | STORE_VAR of string
  | PRINT
  | INPUT
  | CMPNEQ of int
  | CMPEQ of int
  | CMPLT of int
  | CMPGT of int
  | JUMP of int

type bytecode = instruction list

let as_string = function
  | LOAD_CONST i -> sprintf "OP_LOAD_CONST %d" i
  | LOAD_VAR s -> sprintf "OP_LOAD_VAR %s" s
  | ADD -> "OP_ADD"
  | MULTIPLY -> "OP_MULTIPLY"
  | DIVIDE -> "OP_DIVIDE"
  | SUBTRACT -> "OP_SUBTRACT"
  | STORE_VAR s -> sprintf "OP_STORE_VAR %s" s
  | PRINT -> "OP_PRINT"
  | INPUT -> "OP_INPUT"
  | CMPNEQ i -> sprintf "OP_CMPNEQ %d" i
  | CMPEQ i -> sprintf "OP_CMPEQ %d" i
  | CMPLT i -> sprintf "OP_CMPLT %d" i
  | CMPGT i -> sprintf "OP_CMPGT %d" i
  | JUMP i -> sprintf "OP_JMP %d" i
