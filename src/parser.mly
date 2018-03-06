/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

/* Brackets and punctuation */
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET LARRAY RARRAY COMMA COLON

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
%token INT BOOL FLOAT VOID STRING ARRAY MATRIX FUNC

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
%right ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN MATTIMESASSIGN EXPASSIGN MODASSIGN
%left OR
%left AND
%left EQ NEQ
%left LANGLE RANGLE LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD MATTIMES
%right EXP
%right NOT
%left INC DEC

%%

program:
  decls EOF { (List.rev (fst $1), List.rev (snd $1)) }

decls:
   /* nothing */   { ([], []) }
 | decls decl SEMI { (($2 :: fst $1), snd $1) }
 | decls fdecl     { (fst $1, ($2 :: snd $1)) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE stmts_opt RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = $4;
	 body = $7 } }

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
  | FUNC LANGLE LPAREN typ_opt RPAREN COLON typ RANGLE
                             { Func($4, $7) }

typ_opt:
    /* nothing */ { [] }
  | typ_list      { List.rev $1 }

typ_list:
    typ                { [$1] }
  | typ_list COMMA typ { $3 :: $1 }

stmts_opt:
    /* nothing */ { [] }
  | stmt_list     { List.rev $1 }

stmt_list:
    stmt           { [$1] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1 }
  | RETURN expr_opt SEMI                    { Return $2 }
  | LBRACE stmts_opt RBRACE                 { Block($2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN initializer_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5) }
  | decl SEMI                               { Decl($1) }

decl:
    VAR typ ID                           { (Var, $2, $3, Noexpr) }
  | VAR typ ID ASSIGN expr               { (Var, $2, $3, $5) }
  | CREATE typ ID                        { (Create, $2, $3, Noexpr) }
  | CREATE typ ID ASSIGN expr            { (Create, $2, $3, $5) }
  | CREATE typ ID LBRACKET expr RBRACKET { (Create, $2, $3, Empty_Array_Lit($5)) }
  | CREATE typ ID LBRACKET expr RBRACKET LBRACKET expr RBRACKET
                                         { (Create, $2, $3, Empty_Matrix_Lit($5, $8)) }

initializer_opt:
    expr_opt { I_Expr($1) }
  | decl     { I_Decl($1) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

primary_expr:
  /* Literals */
    INT_LIT                    { Int_Lit($1) }
  | FLOAT_LIT	                 { Float_Lit($1) }
  | BOOL_LIT                   { Bool_Lit($1) }
  | STRING_LIT                 { String_Lit($1) }
  | ID                         { Id($1) }

  /* Containers */
  | LARRAY args_opt RARRAY     { Array_Lit(Array.of_list $2) }
  | LBRACKET rows_opt RBRACKET { Matrix_Lit(Array.of_list $2) }

  /* Parentheses */
  | LPAREN expr RPAREN         { $2 }

postfix_expr:
    primary_expr              { $1 }

  /* Array/Matrix Indexing */
  | postfix_expr LBRACKET index RBRACKET
                             { Index_Expr(Sgl_Index($1, $3)) }
  | postfix_expr LBRACKET index COMMA index RBRACKET
                             { Index_Expr(Dbl_Index($1, $3, $5)) }

  /* Function Call */
  | ID LPAREN args_opt RPAREN { Call($1, $3) }

prefix_expr:
    postfix_expr       { $1 }

  /* Unary ops */
  | MINUS prefix_expr { Unop(Neg, $2) }
  | NOT prefix_expr   { Unop(Not, $2) }

expr:
    prefix_expr              { $1 }

  /* Binary ops */
  | expr PLUS   expr         { Binop($1, Add, $3) }
  | expr MINUS  expr         { Binop($1, Sub, $3) }
  | expr TIMES  expr         { Binop($1, Mult, $3) }
  | expr DIVIDE expr         { Binop($1, Div, $3) }
  | expr MATTIMES expr       { Binop($1, MatMult, $3) }
  | expr MOD expr            { Binop($1, Mod, $3) }
  | expr EXP expr            { Binop($1, Exp, $3) }
  | expr EQ     expr         { Binop($1, Equal, $3) }
  | expr NEQ    expr         { Binop($1, Neq, $3) }
  | expr LANGLE expr         { Binop($1, Less, $3) }
  | expr LEQ    expr         { Binop($1, Leq, $3) }
  | expr RANGLE expr         { Binop($1, Greater, $3) }
  | expr GEQ    expr         { Binop($1, Geq, $3) }
  | expr AND    expr         { Binop($1, And, $3) }
  | expr OR     expr         { Binop($1, Or, $3) }

  /* Assignment ops */
  | expr ASSIGN expr         { Assign($1, Noop, $3) }
  | expr PLUSASSIGN expr     { Assign($1, Add, $3) }
  | expr MINUSASSIGN expr    { Assign($1, Sub, $3) }
  | expr TIMESASSIGN expr    { Assign($1, Mult, $3) }
  | expr DIVIDEASSIGN expr   { Assign($1, Div, $3) }
  | expr MATTIMESASSIGN expr { Assign($1, MatMult, $3) }
  | expr MODASSIGN expr      { Assign($1, Mod, $3) }
  | expr EXPASSIGN expr      { Assign($1, Exp, $3) }
  | expr INC                 { Assign($1, Add, One) }
  | expr DEC                 { Assign($1, Sub, One) }
  /* Semantics: check that only IDs/index exprs can be assigned values */

index:
    expr            { Index($1) }
  | expr COLON expr { Slice($1, $3) }
  | COLON expr      { Slice(Int_Lit(0), $2) }
  | expr COLON      { Slice($1, End) }
  | COLON           { Slice(Int_Lit(0), End) }

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
