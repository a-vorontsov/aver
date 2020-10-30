%{
  open Ast
%}

%token <int> INT
%token <string> ID
%token TRUE
%token FALSE LEQ TIMES PLUS
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LET
%token EQUALS
%token IF THEN ELSE
%token FUNC
%token EOF
%token COMMA SEMICOLON

%nonassoc ELSE
%left LEQ
%left PLUS
%left TIMES

%start <Ast.prog> prog
%%

prog:
  | f = funcs; EOF { f }
  ;

funcs: f = list(func) { f }
  ;

func:
  | FUNC; x = ID; LPAREN; ps = params; RPAREN; b = block { Func (x, Params ps, Exprs b) }
  ;

block:
  | LBRACE; ls = lines; RBRACE { ls }
  ;

lines: ls = list(line) { ls }
  ;

line:
  | e = expr; SEMICOLON { e }
  ;

params: ps = separated_list(COMMA, param) { ps }
  ;

param:
  | x = ID { x }
  ;

expr:
  | i = INT { Int i }
  | x = ID { Var x }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | e1 = expr; LEQ; e2 = expr { Binop (Leq, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) }
  | e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
  | LET; x = ID; EQUALS; e = expr { Let (x, e) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | LPAREN; e = expr; RPAREN { e }
  ;
