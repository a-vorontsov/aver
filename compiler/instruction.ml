open Printf

type instruction =
  | LOAD_CONST of int
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
  | CMPNEQ
  | CMPEQ
  | CMPLT
  | CMPLE
  | CMPGT
  | CMPGE
  | CMPAND
  | CMPOR
  | JUMP of int
  | JUMP_TRUE of int
  | MAKE_ARRAY of int
  | MAKE_EMPTY_ARRAY of int
  | LOAD_FROM_ARRAY
  | STORE_TO_ARRAY
  | MAKE_OBJECT of int
  | GET_FIELD of int
  | SET_FIELD of int
  | CALL of int * int
  | MAKE_FUNCTION of int * int
  | HALT
  | RETURN
  | PASS
  | POP

type bytecode = instruction list

let as_string = function
  | LOAD_CONST i -> sprintf "0\t%d" i
  | LOAD_CONST_I i -> sprintf "1\t%d" i
  | LOAD_CONST_F f -> sprintf "2\t%f" f
  | LOAD_CONST_B b -> sprintf "3\t%b" b
  | LOAD_CONST_C c -> sprintf "4\t%c" c
  | LOAD_CONST_S s -> sprintf "5\t%s" s
  | LOAD_VAR i -> sprintf "6\t%d" i
  | ADD -> "7"
  | MULTIPLY -> "8"
  | DIVIDE -> "9"
  | SUBTRACT -> "10"
  | MOD -> "11"
  | STORE_VAR i -> sprintf "12\t%d" i
  | PRINT -> "13"
  | PRINTLN -> "14"
  | INPUT -> "15"
  | CMPNEQ -> "16"
  | CMPEQ -> "17"
  | CMPLT -> "18"
  | CMPLE -> "19"
  | CMPGT -> "20"
  | CMPGE -> "21"
  | CMPAND -> "22"
  | CMPOR -> "23"
  | JUMP i -> sprintf "24\t%d" i
  | JUMP_TRUE i -> sprintf "25\t%d" i
  | MAKE_ARRAY i -> sprintf "26\t%d" i
  | MAKE_EMPTY_ARRAY i -> sprintf "27\t%d" i
  | LOAD_FROM_ARRAY -> "28"
  | STORE_TO_ARRAY -> "29"
  | MAKE_OBJECT i -> sprintf "30\t%d" i
  | GET_FIELD i -> sprintf "31\t%d" i
  | SET_FIELD i -> sprintf "32\t%d" i
  | CALL (i, i') -> sprintf "33\t%d\t%d" i i'
  | MAKE_FUNCTION (i, i') -> sprintf "34\t%d\t%d" i i'
  | HALT -> "35"
  | RETURN -> "36"
  | PASS -> "37"
  | POP -> "38"
