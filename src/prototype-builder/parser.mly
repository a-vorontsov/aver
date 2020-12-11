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
%token PRINT INPUT
%token IF ELSE
%token PASS
%token WHILE
%token EOF
%token SEMICOLON

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

prog:
  | s = stmt* EOF { s }

stmt:
  | s = assignment SEMICOLON { Assign s }
  | PRINT e = expr SEMICOLON { Print e }
  | IF LPAREN c = condition RPAREN LBRACE s1 = stmt* RBRACE ELSE LBRACE s2 = stmt* RBRACE { If (c, s1, s2) }
  | WHILE LPAREN c = condition RPAREN LBRACE s = stmt* RBRACE { While (c, s) }
  | PASS SEMICOLON { Pass }

condition:
  | e1 = expr o = bop e2 = expr { Bincond (o, e1, e2) }

assignment:
  | x = ID; EQUALS; e = expr { x, e }

expr:
  | i = INT { Int i }
  | x = ID { Var x }
  | INPUT { Input }
  | e1 = expr o = op e2 = expr { Binop (o, e1, e2) }
  | LPAREN e = expr RPAREN { e }
