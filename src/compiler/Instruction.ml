open Printf

type instruction =
  | LOAD_CONST_I of int
  | LOAD_CONST_F of float
  | LOAD_CONST_B of bool
  | LOAD_CONST_C of char
  | LOAD_CONST_S of string
  | LOAD_VAR of int
  | ADD
  | MULTIPLY
  | DIVIDE
  | SUBTRACT
  | MOD
  | STORE_VAR of int
  | PRINT
  | PRINTLN
  | INPUT
  | CMPNEQ of int
  | CMPEQ of int
  | CMPLT of int
  | CMPLE of int
  | CMPGT of int
  | CMPGE of int
  | JUMP of int
  | CALL of int * int
  | MAKE_FUNCTION of int * int
  | HALT
  | RETURN
  | PASS

type bytecode = instruction list

let as_string = function
  | LOAD_CONST_I i -> sprintf "0\t%d" i
  | LOAD_CONST_F f -> sprintf "1\t%f" f
  | LOAD_CONST_B b -> sprintf "2\t%b" b
  | LOAD_CONST_C c -> sprintf "3\t%c" c
  | LOAD_CONST_S s -> sprintf "4\t%s" s
  | LOAD_VAR i -> sprintf "5\t%d" i
  | ADD -> "6"
  | MULTIPLY -> "7"
  | DIVIDE -> "8"
  | SUBTRACT -> "9"
  | MOD -> "10"
  | STORE_VAR i -> sprintf "11\t%d" i
  | PRINT -> "12"
  | PRINTLN -> "13"
  | INPUT -> "14"
  | CMPNEQ i -> sprintf "15\t%d" i
  | CMPEQ i -> sprintf "16\t%d" i
  | CMPLT i -> sprintf "17\t%d" i
  | CMPLE i -> sprintf "18\t%d" i
  | CMPGT i -> sprintf "19\t%d" i
  | CMPGE i -> sprintf "20\t%d" i
  | JUMP i -> sprintf "21\t%d" i
  | CALL (i, i') -> sprintf "22\t%d\t%d" i i'
  | MAKE_FUNCTION (i, i') -> sprintf "23\t%d\t%d" i i'
  | HALT -> "24"
  | RETURN -> "25"
  | PASS -> "26"
