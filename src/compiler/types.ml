type type_expr =
  | T_int
  | T_float
  | T_bool
  | T_char
  | T_string
  | T_void
  | T_array of type_expr

let rec get_array_base_type t =
  match t with
  | T_array t' -> get_array_base_type t'
  | T_int | T_float | T_bool | T_char | T_string | T_void -> t

let rec type_to_string = function
  | T_int -> "int"
  | T_float -> "float"
  | T_bool -> "bool"
  | T_char -> "char"
  | T_string -> "string"
  | T_void -> "void"
  | T_array t -> Printf.sprintf "%s[]" (type_to_string t)
