%{
  open Ast
%}

%token <int> INT
%token <string> ID
%token TIMES PLUS DIV SUB
%token EQUALS
%token LPAREN RPAREN
%token EOF

%left DIV
%left PLUS
%left TIMES
%left SUB

%start <Ast.prog> prog
%%

prog:
  | e = stmts; EOF {e}
  ;

stmts: s = list(stmt) { s }
  ;

stmt:
  | s = assignment { Astmt s }
  | a = expr { Aexpr a }
  ;

assignment: x = ID; EQUALS; e = expr { x, e }
  ;

expr:
  | i = INT { Int i }
  | x = ID { Var x }
  | e1 = expr; DIV; e2 = expr { Binop (Div, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) }
  | e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
  | e1 = expr; SUB; e2 = expr { Binop (Sub, e1, e2) }
  | LPAREN; e = expr; RPAREN { e }
  ;
