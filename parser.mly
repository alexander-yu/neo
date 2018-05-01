/* Ocamlyacc parser for Neo */

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
%token INT BOOL FLOAT VOID STRING ARRAY MATRIX FUNC AUTO

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
%nonassoc LANGLE RANGLE LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD MATTIMES
%right EXP
%right NOT
%nonassoc INC DEC

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
    typ ID                   { [(Nokw, $1, $2, Noexpr)] }
  | formal_list COMMA typ ID { (Nokw, $3, $4, Noexpr) :: $1 }

typ:
    INT                      { Int }
  | BOOL                     { Bool }
  | FLOAT                    { Float }
  | STRING                   { String }
  | VOID                     { Void  }
  | ARRAY LANGLE typ RANGLE  { Array $3 }
  | MATRIX LANGLE typ RANGLE { Matrix $3 }
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

/* Implement for loops as while loops */
for_loop:
    FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
      { Block [Expr $3; While($5, Block [$9; Expr $7])] }
  | FOR LPAREN decl SEMI expr SEMI expr_opt RPAREN stmt
      /* Implement declaration initializer as declaration
       * followed by loop in a block */
      { Block [Decl $3; While($5, Block [$9; Expr $7])] }

/* This is to prevent if/else statements from containing
 * single-line declarations in their bodies; declarations
 * can be used in if/else bodies only if they're inside of
 * a block. This is due to scope reasons; an if/else statement
 * should not affect its containing scope (i.e. adding new
 * variables), so blocks are okay but declarations are not. */
nondecl_stmt:
    expr SEMI                               { Expr $1 }
  | RETURN expr_opt SEMI                    { Return $2 }
  | LBRACE stmts_opt RBRACE                 { Block $2 }
  | IF LPAREN expr RPAREN nondecl_stmt %prec NOELSE
                                            { If($3, $5, Block []) }
  | IF LPAREN expr RPAREN nondecl_stmt ELSE nondecl_stmt
                                            { If($3, $5, $7) }
  | for_loop                                { $1 }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5) }

stmt:
    nondecl_stmt { $1 }
  | decl SEMI    { Decl $1 }

decl:
    VAR typ ID                           { (Var, $2, $3, Noexpr) }
  | VAR typ ID ASSIGN expr               { (Var, $2, $3, $5) }
  | CREATE typ ID                        { (Create, $2, $3, Noexpr) }
  | CREATE typ ID ASSIGN expr            { (Create, $2, $3, $5) }
  | CREATE typ ID LBRACKET expr RBRACKET { (Create, $2, $3,
                                            Empty_Array(typ_of_container $2, $5)) }
  | CREATE typ ID LBRACKET expr RBRACKET LBRACKET expr RBRACKET
                                         { (Create, $2, $3,
                                            Empty_Matrix(typ_of_container $2, $5, $8)) }
  | AUTO ID ASSIGN expr                  { (Auto, Notyp, $2, $4) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

primary_expr:
  /* Literals */
    INT_LIT                    { Int_Lit $1 }
  | FLOAT_LIT	                 { Float_Lit $1 }
  | BOOL_LIT                   { Bool_Lit $1 }
  | STRING_LIT                 { String_Lit $1 }
  | ID                         { Id $1 }

  /* Containers */
  | LARRAY args_opt RARRAY     { Array_Lit(Array.of_list $2) }
  | LBRACKET rows_opt RBRACKET { Matrix_Lit(Array.of_list $2) }

  /* Parentheses */
  | LPAREN expr RPAREN         { $2 }

postfix_expr:
    primary_expr              { $1 }

  /* Array/Matrix Indexing */
  | postfix_expr LBRACKET index RBRACKET
                              {
                                match $3 with
                                  | Index _ -> Index_Expr(Sgl_Index($1, $3))
                                  | Slice _ -> Slice_Expr(Sgl_Slice($1, $3))
                              }
  | postfix_expr LBRACKET index COMMA index RBRACKET
                              {
                                match ($3, $5) with
                                  | Index _, Index _ ->
                                      Index_Expr(Dbl_Index($1, $3, $5))
                                  | Slice _, Index _ ->
                                      Slice_Expr(Dbl_Slice($1, $3, index_to_slice($5)))
                                  | Index _, Slice _ ->
                                      Slice_Expr(Dbl_Slice($1, index_to_slice($3), $5))
                                  | Slice _, Slice _ ->
                                      Slice_Expr(Dbl_Slice($1, $3, $5))
                              }
  /* Function Call */
  | postfix_expr LPAREN args_opt RPAREN { Call($1, $3) }

prefix_expr:
    postfix_expr      { $1 }

  /* Unary ops */
  | MINUS prefix_expr { Unop(Neg, $2) }
  | NOT prefix_expr   { Unop(Not, $2) }

expr:
    prefix_expr              { $1 }

  /* Binary ops */
  | expr PLUS   expr         { Binop($1, Add, $3, false) }
  | expr MINUS  expr         { Binop($1, Sub, $3, false) }
  | expr TIMES  expr         { Binop($1, Mult, $3, false) }
  | expr DIVIDE expr         { Binop($1, Div, $3, false) }
  | expr MATTIMES expr       { Binop($1, MatMult, $3, false) }
  | expr MOD expr            { Binop($1, Mod, $3, false) }
  | expr EXP expr            { Binop($1, Exp, $3, false) }
  | expr EQ     expr         { Binop($1, Equal, $3, false) }
  | expr NEQ    expr         { Binop($1, Neq, $3, false) }
  | expr LANGLE expr         { Binop($1, Less, $3, false) }
  | expr LEQ    expr         { Binop($1, Leq, $3, false) }
  | expr RANGLE expr         { Binop($1, Greater, $3, false) }
  | expr GEQ    expr         { Binop($1, Geq, $3, false) }
  | expr AND    expr         { Binop($1, And, $3, false) }
  | expr OR     expr         { Binop($1, Or, $3, false) }

  /* Assignment ops */
  | expr ASSIGN expr         { Assign($1, $3) }
  | expr PLUSASSIGN expr     { Assign($1, Binop($1, Add, $3, true)) }
  | expr MINUSASSIGN expr    { Assign($1, Binop($1, Sub, $3, true)) }
  | expr TIMESASSIGN expr    { Assign($1, Binop($1, Mult, $3, true)) }
  | expr DIVIDEASSIGN expr   { Assign($1, Binop($1, Div, $3, true)) }
  | expr MATTIMESASSIGN expr { Assign($1, Binop($1, MatMult, $3, true)) }
  | expr MODASSIGN expr      { Assign($1, Binop($1, Mod, $3, true)) }
  | expr EXPASSIGN expr      { Assign($1, Binop($1, Exp, $3, true)) }
  | expr INC                 { Assign($1, Binop($1, Add, One, true)) }
  | expr DEC                 { Assign($1, Binop($1, Sub, One, true)) }
  /* Semantics: check that only IDs/index exprs can be assigned values */

index:
    expr            { Index $1 }
  | expr COLON expr { Slice($1, $3) }
  | COLON expr      { Slice(Int_Lit 0, $2) }
  | expr COLON      { Slice($1, End) }
  | COLON           { Slice(Int_Lit 0, End) }

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
