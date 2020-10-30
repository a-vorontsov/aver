%{
  open Ast
%}

%token <int> INT
%token TIMES PLUS DIV SUB
%token LPAREN RPAREN
%token EOF

%left DIV
%left PLUS
%left TIMES
%left SUB

%start <Ast.expr> prog
%%

prog:
  | e = expr; EOF {e}
  ;

expr:
  | i = INT { Int i }
  | e1 = expr; DIV; e2 = expr { Binop (Div, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) }
  | e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
  | e1 = expr; SUB; e2 = expr { Binop (Sub, e1, e2) }
  | LPAREN; e = expr; RPAREN { e }
  ;
