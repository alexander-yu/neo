(* Ocamllex scanner for MicroC *)

{
  open Parser
  let fail ch = raise (Failure("illegal character " ^ Char.escaped char))
}

let digit = ['0' - '9']
let letter = ['a'-'z' 'A'-'Z']
let digits = digit+
let simple_char = [' '-'!' '#'-'&' '('-'[' ']'-'~']
let escape_char = ['t' 'r' 'n' '\'' '\"' '\\']
let types = "int" | "bool" | "float" | "string" | "void" | "array" | "matrix"

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&"      { AND }
| "|"      { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| types as t { typ t }
| "array<" { ARRAY; LANGLE; container 1 lexbuf }
| "matrix<" { MATRIX; LANGLE; container 1 lexbuf }
| "true"   { BOOL_LIT(true)  }
| "false"  { BOOL_LIT(false) }
| digits as lxm { INT_LIT(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLOAT_LIT(lxm) }
| letter (letter | digit | '_')* as lxm { ID(lxm) }
| '\"' ((simple_char | '\\' escape_char)* as lxm) '\"' { STRING_LIT(lxm) }
| eof { EOF }
| _ as ch { fail ch }

and typ = parse
  "int"    { INT; token lexbuf }
| "bool"   { BOOL; token lexbuf }
| "float"  { FLOAT; token lexbuf }
| "string" { STRING; token lexbuf }
| "void"   { VOID; token lexbuf }
| "array"  { ARRAY; token lexbuf }
| "matrix" { MATRIX; token lexbuf }
| _ as ch  { fail ch }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and container n = parse
  "<" { LANGLE; container (n + 1) lexbuf }
| ">" { RANGLE; if n > 0 then container (n - 1) lexbuf else token lexbuf }
| types as t { typ t; container n lexbuf }
| _ as ch { fail ch }
