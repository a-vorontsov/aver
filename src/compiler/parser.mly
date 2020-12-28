%{
  open Ast
%}

%token <int> INT
%token <string> ID
%token TIMES PLUS DIV MINUS MOD
%token EQUALS
%token BEQUALS BNEQUALS GT LT
%token LPAREN RPAREN
%token LBRACE RBRACE
%token COLON SEMICOLON COMMA
%token LET
%token PRINT INPUT
%token IF ELSE
%token PASS
%token WHILE
%token FUNC RETURN
%token EOF
%token T_INT T_BOOL T_CHAR T_STRING T_VOID

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
  | LT { LessThan }

%inline op:
  | DIV { Div }
  | TIMES { Mult }
  | PLUS { Add }
  | MINUS { Sub }
  | MOD { Mod }

%inline prim_type:
  | T_INT { T_int }
  | T_BOOL { T_bool }
  | T_CHAR  { T_char }
  | T_STRING { T_string }
  | T_VOID { T_void }

prog:
  | f = func* EOF { f }

func:
  | FUNC x = ID ps = params COLON prim_type b = block { Func (x, ps, b) }

block:
  | LBRACE ls = line* RBRACE { ls }

line:
  | s = stmt { s }

params:
  | LPAREN ps = param_list RPAREN { ps }

param_list:
  | ps = separated_list(COMMA, param) { ps }

param:
  | x = ID COLON t = prim_type { x, t }

stmt:
  | LET d = declaration SEMICOLON { Declare d }
  | a = assignment SEMICOLON { Assign a }
  | PRINT e = expr SEMICOLON { Print e }
  | IF LPAREN c = condition RPAREN s1 = block ELSE s2 = block { If (c, s1, s2) }
  | WHILE LPAREN c = condition RPAREN s = block { While (c, s) }
  | PASS SEMICOLON { Pass }
  | RETURN e = expr SEMICOLON { Return e }
  | f = function_call SEMICOLON { Call f }

condition:
  | e1 = expr o = bop e2 = expr { Bincond (o, e1, e2) }

declaration:
  | x = ID COLON t = prim_type EQUALS e = expr {x, t, Some e }
  | x = ID COLON t = prim_type { x, t, None }

assignment:
  | x = ID EQUALS e = expr { x, e }

expr:
  | i = INT { Num i }
  | x = ID { Var x }
  | INPUT { Input }
  | f = function_call { AssignCall f }
  | e1 = expr o = op e2 = expr { Binop (o, e1, e2) }
  | LPAREN e = expr RPAREN { e }

function_call:
 | x = ID LPAREN p = separated_list(COMMA, expr) RPAREN { x, p }
