type prim_type = T_int | T_float | T_bool | T_char | T_string | T_void

let type_to_string = function
  | T_int -> "int"
  | T_float -> "float"
  | T_bool -> "bool"
  | T_char -> "char"
  | T_string -> "string"
  | T_void -> "void"
