%{
  open Ast
%}

%token <int> INT
%token <string> ID
%token TIMES PLUS DIV MINUS
%token EQUALS
%token LPAREN RPAREN
%token PRINT
%token INPUT
%token EOF
%token SEMICOLON

%left DIV
%left PLUS
%left TIMES
%left MINUS
%nonassoc UMINUS

%start <Ast.prog> prog
%%

prog:
  | e = stmts; EOF {e}

stmts: s = list(stmt) { s }

stmt:
  | s = assignment; SEMICOLON { Astmt s }
  | PRINT; a = expr; SEMICOLON { Print a }

assignment:
  | x = ID; EQUALS; e = expr { x, e }

expr:
  | i = INT { Int i }
  | MINUS e = INT %prec UMINUS { Int (-e)  }
  | x = ID { Var x }
  | INPUT { Input }
  | e1 = expr; o = op; e2 = expr { Binop (o, e1, e2) }
  | LPAREN; e = expr; RPAREN { e }

%inline op:
  | DIV { Div }
  | TIMES { Mult }
  | PLUS { Add }
  | MINUS { Sub }
