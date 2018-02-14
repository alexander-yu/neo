/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token NOT EQ NEQ LEQ GEQ AND OR
%token LANGLE RANGLE
%token RETURN IF ELSE FOR WHILE
%token INT BOOL FLOAT VOID STRING ARRAY MATRIX
%token <int> INT_LIT
%token <bool> BOOL_LIT
%token <string> ID FLOAT_LIT
%token <string> STRING_LIT
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG


%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], []) }
 | decls vdecl   { (($2 :: fst $1), snd $1) }
 | decls fdecl   { (fst $1, ($2 :: snd $1)) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1, $2)] }
  | formal_list COMMA typ ID { ($3, $4) :: $1 }

typ:
    INT                      { Int }
  | BOOL                     { Bool }
  | FLOAT                    { Float }
  | STRING                   { String }
  | VOID                     { Void  }
  | ARRAY LANGLE typ RANGLE  { Array($3) }
  | MATRIX LANGLE typ RANGLE { Matrix($3) }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1 }
  | RETURN expr_opt SEMI                    { Return $2 }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    INT_LIT                    { Int_Lit($1) }
  | FLOAT_LIT	                 { Float_Lit($1) }
  | BOOL_LIT                   { Bool_Lit($1) }
  | STRING_LIT                 { String_Lit($1) }
  | ID                         { Id($1) }
  | LBRACE args_opt RBRACE     { Array_Lit($2) }
  | LBRACKET rows_opt RBRACKET { Matrix_Lit($2) }
  | expr PLUS   expr           { Binop($1, Add, $3) }
  | expr MINUS  expr           { Binop($1, Sub, $3) }
  | expr TIMES  expr           { Binop($1, Mult, $3) }
  | expr DIVIDE expr           { Binop($1, Div, $3) }
  | expr EQ     expr           { Binop($1, Equal, $3) }
  | expr NEQ    expr           { Binop($1, Neq, $3) }
  | expr LANGLE expr           { Binop($1, Less, $3) }
  | expr LEQ    expr           { Binop($1, Leq, $3) }
  | expr RANGLE expr           { Binop($1, Greater, $3) }
  | expr GEQ    expr           { Binop($1, Geq, $3) }
  | expr AND    expr           { Binop($1, And, $3) }
  | expr OR     expr           { Binop($1, Or, $3) }
  | MINUS expr %prec NEG       { Unop(Neg, $2) }
  | NOT expr                   { Unop(Not, $2) }
  | ID ASSIGN expr             { Assign($1, $3) }
  | ID LPAREN args_opt RPAREN  { Call($1, $3) }
  | LPAREN expr RPAREN         { $2 }

rows_opt:
    /* nothing */ { [] }
  | rows_list     { List.rev $1 }

rows_list:
    LBRACKET args_opt RBRACKET                 { [$2] }
  | rows_list COMMA LBRACKET args_opt RBRACKET { $4 :: $1 }

args_opt:
    /* nothing */ { [] }
  | args_list     { List.rev $1 }

args_list:
    expr                 { [$1] }
  | args_list COMMA expr { $3 :: $1 }
