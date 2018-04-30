(* Ocamllex scanner for Neo *)

{
  open Parser
  exception Scan_error of string
  let fail ch = raise (Scan_error (Char.escaped ch))
}

let digit = ['0' - '9']
let letter = ['a'-'z' 'A'-'Z']
let digits = digit+
let simple_char = [' '-'!' '#'-'&' '('-'[' ']'-'~']
let escape_char = ['t' 'r' 'n' '\'' '\"' '\\']

rule token = parse
  [' ' '\t' '\r'] { token lexbuf }
| '\n'     { Lexing.new_line lexbuf; token lexbuf }
| "/*"     { comment lexbuf }

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

(* Declaration *)
| "var"       { VAR }
| "create"    { CREATE }
| "auto"      { AUTO }

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
