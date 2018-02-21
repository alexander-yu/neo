/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

/* Brackets and punctuation */
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET LARRAY RARRAY COMMA

/* Binary ops */
%token PLUS MINUS TIMES DIVIDE MATTIMES MOD EXP

/* Assignment ops */
%token ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN MATTIMESASSIGN EXPASSIGN MODASSIGN INC DEC

/* Relational ops */
%token EQ NEQ LEQ GEQ LANGLE RANGLE

/* Logical ops */
%token NOT AND OR

/* Control flow */
%token RETURN IF ELSE FOR WHILE

/* Declaration */
%token VAR CREATE

/* Types */
%token INT BOOL FLOAT VOID STRING ARRAY MATRIX

/* Literals */
%token <int> INT_LIT
%token <bool> BOOL_LIT
%token <string> ID FLOAT_LIT STRING_LIT

/* Misc */
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN MATTIMESASSIGN EXPASSIGN MODASSIGN INC DEC
%left OR
%left AND
%left EQ NEQ
%left LANGLE RANGLE LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD MATTIMES
%right EXP
%right NOT NEG


%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], []) }
 | decls vdecl   { (($2 :: fst $1), snd $1) }
 | decls fdecl   { (fst $1, ($2 :: snd $1)) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmts_opt RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = $8 } }

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
    VAR typ ID SEMI { (Var, $2, $3) }
  | CREATE typ ID SEMI { (Create, $2, $3) }

stmts_opt:
    /* nothing */  { [] }
  | stmt_list { List.rev $1 }

stmt_list:
    stmt      { [$1] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1 }
  | RETURN expr_opt SEMI                    { Return $2 }
  | LBRACE stmts_opt RBRACE                 { Block($2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
  /* Literals */
    INT_LIT                    { Int_Lit($1) }
  | FLOAT_LIT	                 { Float_Lit($1) }
  | BOOL_LIT                   { Bool_Lit($1) }
  | STRING_LIT                 { String_Lit($1) }
  | ID                         { Id($1) }

  /* Containers */
  | LARRAY args_opt RARRAY     { Array_Lit(Array.of_list $2) }
  | LBRACKET rows_opt RBRACKET { Matrix_Lit(Array.of_list $2) }

  /* Binary ops */
  | expr PLUS   expr           { Binop($1, Add, $3) }
  | expr MINUS  expr           { Binop($1, Sub, $3) }
  | expr TIMES  expr           { Binop($1, Mult, $3) }
  | expr DIVIDE expr           { Binop($1, Div, $3) }
  | expr MATTIMES expr         { Binop($1, MatMult, $3) }
  | expr MOD expr              { Binop($1, Mod, $3) }
  | expr EXP expr              { Binop($1, Exp, $3) }
  | expr EQ     expr           { Binop($1, Equal, $3) }
  | expr NEQ    expr           { Binop($1, Neq, $3) }
  | expr LANGLE expr           { Binop($1, Less, $3) }
  | expr LEQ    expr           { Binop($1, Leq, $3) }
  | expr RANGLE expr           { Binop($1, Greater, $3) }
  | expr GEQ    expr           { Binop($1, Geq, $3) }
  | expr AND    expr           { Binop($1, And, $3) }
  | expr OR     expr           { Binop($1, Or, $3) }

  /* Unary ops */
  | MINUS expr %prec NEG       { Unop(Neg, $2) }
  | NOT expr                   { Unop(Not, $2) }

  /* Assignment ops */
  | ID ASSIGN expr         { Assign($1, Noop, $3) }
  | ID PLUSASSIGN expr     { Assign($1, Add, $3) }
  | ID MINUSASSIGN expr    { Assign($1, Sub, $3) }
  | ID TIMESASSIGN expr    { Assign($1, Mult, $3) }
  | ID DIVIDEASSIGN expr   { Assign($1, Div, $3) }
  | ID MATTIMESASSIGN expr { Assign($1, MatMult, $3) }
  | ID MODASSIGN expr      { Assign($1, Mod, $3) }
  | ID EXPASSIGN expr      { Assign($1, Exp, $3) }
  | ID INC                 { Assign($1, Add, One) }
  | ID DEC                 { Assign($1, Sub, One) }

  /* Parentheses */
  | ID LPAREN args_opt RPAREN  { Call($1, $3) }
  | LPAREN expr RPAREN         { $2 }

rows_opt:
    /* nothing */ { [] }
  | rows_list     { List.rev $1 }

rows_list:
    LBRACKET args_opt RBRACKET                 { [Array.of_list $2] }
  | rows_list COMMA LBRACKET args_opt RBRACKET { (Array.of_list $4) :: $1 }

args_opt:
    /* nothing */ { [] }
  | args_list     { List.rev $1 }

args_list:
    expr                 { [$1] }
  | args_list COMMA expr { $3 :: $1 }
