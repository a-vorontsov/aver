%{
  open Ast
  open Types
%}

%token <int> INT
%token <string> ID
%token <float> FLOAT
%token <string> STRING
%token <string> STRUCT_ID
%token TIMES PLUS DIV MINUS MOD
%token EQUALS
%token BEQUALS BNEQUALS GT GE LT LE
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LSQUARE RSQUARE
%token SEMICOLON COMMA DOT COLON
%token LET
%token PRINT PRINTLN INPUT
%token IF ELSE
%token PASS
%token WHILE
%token FUNC RETURN STRUCT NULL
%token EOF
%token T_INT T_FLOAT T_BOOL T_CHAR T_STRING T_VOID

%left PLUS MINUS
%left DIV TIMES MOD

%on_error_reduce
  stmt
  expr

%start <Ast.prog> prog
%%

%inline bop:
  | BEQUALS { BEquals }
  | BNEQUALS { BNequals }
  | GT { GreaterThan }
  | GE { GreaterThanEq }
  | LT { LessThan }
  | LE { LessThanEq }

%inline op:
  | DIV { Div }
  | TIMES { Mult }
  | PLUS { Add }
  | MINUS { Sub }
  | MOD { Mod }

%inline prim_type:
  | T_INT { T_int }
  | T_FLOAT { T_float }
  | T_BOOL { T_bool }
  | T_CHAR  { T_char }
  | T_STRING { T_string }
  | T_VOID { T_void }
  | s = STRUCT_ID { T_obj (s) }

any_type:
  | t = prim_type { t }
  | a = array_type { a }

array_type:
  | a = array_type LSQUARE RSQUARE { T_array (a) }
  | t = prim_type LSQUARE RSQUARE { T_array (t) }

prog:
  | s = _struct* f = func* EOF { s,f }

_struct:
  | STRUCT x = STRUCT_ID LBRACE s = struct_field* RBRACE { Struct ($startpos, x, s) }

struct_field:
  | x = ID COLON t = any_type SEMICOLON { StructField ($startpos, x, t) }

func:
  | FUNC x = ID ps = params t = any_type b = block { Func ($startpos, x, t, ps, b) }

block:
  | LBRACE ls = line* RBRACE { ls }

line:
  | s = stmt { s }

params:
  | LPAREN ps = param_list RPAREN { ps }

param_list:
  | ps = separated_list(COMMA, param) { ps }

param:
  | x = ID t = any_type { $startpos, x, t }

stmt:
  | s = single_stmt SEMICOLON { s }
  | IF LPAREN c = condition RPAREN s1 = block ELSE s2 = block { If ($startpos, c, s1, s2) }
  | WHILE LPAREN c = condition RPAREN s = block { While ($startpos, c, s) }

single_stmt:
  | LET d = declaration { Declare ($startpos, d) }
  | a = assignment { Assign ($startpos, a) }
  | a = array_assign { ArrayAssign ($startpos, a) }
  | PRINT e = expr { Print ($startpos, e) }
  | PRINTLN e = expr { Println ($startpos, e) }
  | PASS { Pass $startpos }
  | RETURN e = expr { Return ($startpos, e) }
  | f = function_call { Call ($startpos, f) }

condition:
  | e1 = expr o = bop e2 = expr { Bincond ($startpos, o, e1, e2) }

declaration:
  | x = ID t = any_type EQUALS e = expr { x, Some t, Some e }
  | x = ID EQUALS e = expr { x, None, Some e }
  | x = ID t = any_type { x, Some t, None }

assignment:
  | x = identifier EQUALS e = expr { x, e }

identifier:
  | x = ID { Var x }
  | obj = ID DOT field = ID { ObjField (obj, field) }

expr:
  | i = INT { Num ($startpos, i) }
  | f = FLOAT { FNum ($startpos, f) }
  | s = STRING { Str ($startpos, s) }
  | x = identifier { Identifier ($startpos, x) }
  | NULL { Null $startpos }
  | s = struct_init { s }
  | LSQUARE a = separated_list(COMMA, expr) RSQUARE { Array ($startpos, a) }
  | a = array_dec { ArrayDec($startpos, a) }
  | a = array_access { ArrayAccess ($startpos, a) }
  | INPUT { Input $startpos }
  | f = function_call { AssignCall ($startpos, f) }
  | e1 = expr o = op e2 = expr { Binop ($startpos, o, e1, e2) }
  | LPAREN e = expr RPAREN { e }

array_access:
  | x = ID LSQUARE e = expr RSQUARE { x, e }

array_assign:
  | a = array_access EQUALS e = expr { a, e }

array_dec:
  | a = array_dec LSQUARE i = INT RSQUARE { MultiDim (a, i) }
  | t = prim_type LSQUARE i = INT RSQUARE { SingleDim (t, i) }

struct_init:
  | s = STRUCT_ID LBRACE f = struct_field_init* RBRACE { StructInit ($startpos, s, f) }

struct_field_init:
  | x = ID EQUALS e = expr SEMICOLON { x,e }

function_call:
 | x = ID LPAREN p = separated_list(COMMA, expr) RPAREN { x, p }
