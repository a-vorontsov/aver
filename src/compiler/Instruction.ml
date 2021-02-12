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
  | CMPNEQ of int
  | CMPEQ of int
  | CMPLT of int
  | CMPLE of int
  | CMPGT of int
  | CMPGE of int
  | JUMP of int
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
  | CMPNEQ i -> sprintf "16\t%d" i
  | CMPEQ i -> sprintf "17\t%d" i
  | CMPLT i -> sprintf "18\t%d" i
  | CMPLE i -> sprintf "19\t%d" i
  | CMPGT i -> sprintf "20\t%d" i
  | CMPGE i -> sprintf "21\t%d" i
  | JUMP i -> sprintf "22\t%d" i
  | MAKE_ARRAY i -> sprintf "23\t%d" i
  | MAKE_EMPTY_ARRAY i -> sprintf "24\t%d" i
  | LOAD_FROM_ARRAY -> "25"
  | STORE_TO_ARRAY -> "26"
  | MAKE_OBJECT i -> sprintf "27\t%d" i
  | GET_FIELD i -> sprintf "28\t%d" i
  | SET_FIELD i -> sprintf "29\t%d" i
  | CALL (i, i') -> sprintf "30\t%d\t%d" i i'
  | MAKE_FUNCTION (i, i') -> sprintf "31\t%d\t%d" i i'
  | HALT -> "32"
  | RETURN -> "33"
  | PASS -> "34"
