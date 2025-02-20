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
%token BAND BOR
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
%token EOF TILDE
%token T_INT T_FLOAT T_BOOL T_CHAR T_STRING T_VOID T_GENERIC

%left BOR
%left BAND
%left BEQUALS BNEQUALS GT GE LT LE
%left PLUS MINUS
%left DIV TIMES MOD

%start <Ast.prog> prog
%%

%inline binop:
    | BOR { BOr }
    | BAND { BAnd }
    | BEQUALS { BEquals }
    | BNEQUALS { BNequals }
    | GT { GreaterThan }
    | GE { GreaterThanEq }
    | LT { LessThan }
    | LE { LessThanEq }
    | DIV { Div }
    | TIMES { Mult }
    | PLUS { Add }
    | MINUS { Sub }
    | MOD { Mod }

%inline primitive_type:
    | T_INT { T_int }
    | T_FLOAT { T_float }
    | T_BOOL { T_bool }
    | T_CHAR  { T_char }
    | T_STRING { T_string }
    | T_VOID { T_void }
    | T_GENERIC { T_generic }

any_type:
    | _struct = struct_type { _struct }
    | array = array_type { array }
    | primitive = primitive_type { primitive }

struct_type:
    | s = STRUCT_ID maybe_param = parameterised_type? { T_obj (s, maybe_param) }

array_type:
    | a = array_type LSQUARE RSQUARE { T_array (a) }
    | t = primitive_type LSQUARE RSQUARE { T_array (t) }

generic_type:
    | TILDE T_GENERIC TILDE { Generic }

parameterised_type:
    | TILDE t = any_type TILDE { t }

_struct:
    | STRUCT id = STRUCT_ID maybe_generic = generic_type? LBRACE s = struct_field* RBRACE { Struct ($startpos, id, maybe_generic, s) }

struct_field:
    | id = ID COLON t = any_type SEMICOLON { StructField ($startpos, id, t) }

prog:
    | structs = _struct* functions = func* EOF { structs, functions }

func:
    | FUNC function_identifier = ID maybe_generic = generic_type? function_params = params function_return_type = any_type function_body = block {
        Func (
            $startpos,
            function_identifier,
            maybe_generic,
            function_return_type,
            function_params,
            function_body
        )
    }

params:
    LPAREN parameters = separated_list(COMMA, param) RPAREN { parameters }

param:
    | x = ID t = any_type { $startpos, x, t }

block:
    | LBRACE statements = statement* RBRACE { statements }

statement:
    | s = single_line_statement SEMICOLON { s }
    | i = if_statement { i }
    | w = while_statement { w }

if_statement:
    | IF LPAREN e = expr RPAREN s1 = block ELSE s2 = block { If ($startpos, e, s1, Some(s2)) }
    | IF LPAREN e = expr RPAREN s1 = block { If ($startpos, e, s1, None) }

while_statement:
    | WHILE LPAREN e = expr RPAREN s = block { While ($startpos, e, s) }

single_line_statement:
    | d = declaration { d }
    | a = assignment { a }
    | a = array_assignment { a }

    | p = print { p }
    | r = return { r }

    | p = pass { p }

    | f = function_call { f }

function_call:
    | x = ID maybe_param = parameterised_type? LPAREN p = separated_list(COMMA, expr) RPAREN {
        Call (
            $startpos,
            x,
            maybe_param,
            p
        )
    }

declaration:
    | LET d = declare { d }

declare:
    | x = ID t = any_type EQUALS e = expr { Declare ($startpos, x, Some t, Some e) }
    | x = ID EQUALS e = expr { Declare ($startpos, x, None, Some e) }
    | x = ID t = any_type { Declare ($startpos, x, Some t, None) }

assignment:
    | id = identifier EQUALS e = expr { Assign ($startpos, id, e) }

expr:
    | LPAREN e = expr RPAREN { e }
    | i = INT { Num ($startpos, i) }
    | f = FLOAT { FNum ($startpos, f) }
    | s = STRING { Str ($startpos, s) }
    | x = identifier { Identifier ($startpos, x) }

    | s = STRUCT_ID maybe_param = parameterised_type? LBRACE f = struct_field_init* RBRACE { StructInit ($startpos, s, maybe_param, f) }

    | LSQUARE a = separated_list(COMMA, expr) RSQUARE { Array ($startpos, a) }
    | a = array_dec { ArrayDec($startpos, a) }
    | a = array_access { ArrayAccess ($startpos, a) }

    | NULL { Null $startpos }
    | INPUT { Input $startpos }

    | x = ID maybe_param = parameterised_type? LPAREN p = separated_list(COMMA, expr) RPAREN { AssignCall ($startpos, x, maybe_param, p) }

    | e1 = expr b = binop e2 = expr { Binop($startpos, b, e1, e2) }

identifier:
    | id = ID { Var id }
    | obj = ID DOT fields = separated_list(DOT, ID) { ObjField (obj, fields) }

array_access:
    | id = identifier indices = nonempty_list(delimited(LSQUARE, expr, RSQUARE)) { id, indices }

array_assignment:
    | a = array_access EQUALS e = expr { ArrayAssign ($startpos, a, e) }

array_dec:
    | a = array_dec LSQUARE i = INT RSQUARE { MultiDim (a, i) }
    | t = primitive_type LSQUARE i = INT RSQUARE { SingleDim (t, i) }

struct_field_init:
    | id = ID EQUALS e = expr SEMICOLON { id, e }

print:
    | PRINT e = expr { Print ($startpos, e) }
    | PRINTLN e = expr { Println ($startpos, e) }

return:
    | RETURN e = expr { Return ($startpos, e) }

pass:
    | PASS { Pass $startpos }
