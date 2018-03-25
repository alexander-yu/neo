(* Ocamllex scanner for Neo *)

{
  open Parser
  let fail ch = raise (Failure("illegal character " ^ Char.escaped ch))
}

let digit = ['0' - '9']
let letter = ['a'-'z' 'A'-'Z']
let digits = digit+
let simple_char = [' '-'!' '#'-'&' '('-'[' ']'-'~']
let escape_char = ['t' 'r' 'n' '\'' '\"' '\\']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)

(* Brackets *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| "{|"     { LARRAY }
| "|}"     { RARRAY }

(* Punctuation *)
| ';'      { SEMI }
| ','      { COMMA }
| ':'      { COLON }

(* Binary ops *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '@'      { MATTIMES }
| '^'      { EXP }
| '%'      { MOD }

(* Assignment ops *)
| '='      { ASSIGN }
| "+="     { PLUSASSIGN }
| "-="     { MINUSASSIGN }
| "*="     { TIMESASSIGN }
| "/="     { DIVIDEASSIGN }
| "@="     { MATTIMESASSIGN }
| "^="     { EXPASSIGN }
| "%="     { MODASSIGN }
| "++"     { INC }
| "--"     { DEC }

(* Relational ops *)
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LANGLE }
| "<="     { LEQ }
| ">"      { RANGLE }
| ">="     { GEQ }

(* Logical ops *)
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }

(* Control flow *)
| "if"      { IF }
| "else"    { ELSE }
| "for"     { FOR }
| "while"   { WHILE }
| "return"  { RETURN }
| "try"     { TRY }
| "catch"   { CATCH }
| "protest" { PROTEST }

(* Declaration *)
| "var"       { VAR }
| "create"    { CREATE }
| "exception" { EXCEPTION }

(* Types *)
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "string" { STRING }
| "void"   { VOID }
| "array"  { ARRAY }
| "matrix" { MATRIX }
| "func"   { FUNC }

(* Literals *)
| "True"   { BOOL_LIT(true)  }
| "False"  { BOOL_LIT(false) }
| digits as lxm { INT_LIT(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLOAT_LIT(lxm) }
| letter (letter | digit | '_')* as lxm { ID(lxm) }
| '\"' ((simple_char | '\\' escape_char)* as lxm) '\"' { STRING_LIT(lxm) }

(* Misc *)
| eof { EOF }
| _ as ch { fail ch }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
