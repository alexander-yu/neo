(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | MatMult | Mod | Exp | Noop

type uop = Neg | Not

type typ = Int | Bool | Float | String | Void |
           Array of typ | Matrix of typ

type bind = typ * string

type decl_kw = Var | Create

type expr =
    Id of string
  | Int_Lit of int
  | Float_Lit of string
  | Bool_Lit of bool
  | String_Lit of string
  | Array_Lit of expr array
  | Matrix_Lit of expr array array
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * op * expr
  | Call of string * expr list
  | One
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Decl of decl_kw * typ * string * expr

  type global = decl_kw * typ * string * expr

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
  }

type program = global list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | MatMult -> "@"
  | Mod -> "%"
  | Exp -> "^"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&"
  | Or -> "|"
  | Noop -> ""

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let string_of_decl_kw = function
    Var -> "var"
  | Create -> "create"

let rec string_of_typ = function
  Int -> "int"
| Bool -> "bool"
| Float -> "float"
| String -> "string"
| Void -> "void"
| Array(t) -> "array<" ^ string_of_typ t ^ ">"
| Matrix(t) -> "matrix<"  ^ string_of_typ t ^ ">"

let rec string_of_expr = function
    Int_Lit(l) -> string_of_int l
  | Float_Lit(l) -> l
  | Bool_Lit(true) -> "true"
  | Bool_Lit(false) -> "false"
  | String_Lit(l) -> "\"" ^ l ^ "\""
  | Array_Lit(l) -> string_of_array l
  | Matrix_Lit(l) -> string_of_matrix l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, o, e) -> v ^ " " ^ string_of_op o ^ "= " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | One -> "[1]"
  | Noexpr -> ""

and string_of_array arr =
  "{|" ^ String.concat ", " (Array.to_list (Array.map string_of_expr arr)) ^ "|}"

and string_of_exprs exprs =
  String.concat ", " (List.map string_of_expr exprs)

and string_of_row row =
  "[" ^ String.concat ", " (Array.to_list (Array.map string_of_expr row)) ^ "]"

and string_of_matrix matrix =
  "[" ^ String.concat ", " (Array.to_list (Array.map string_of_row matrix)) ^ "]"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Decl(kw, t, id, expr) -> match expr with
    Noexpr -> string_of_decl_kw kw ^ " " ^ string_of_typ t ^ " " ^ id ^ ";\n"
    | _ -> string_of_decl_kw kw ^ " " ^ string_of_typ t ^ " " ^ id ^ " = " ^
        string_of_expr expr ^ ";\n"

let string_of_global (kw, t, id, expr) = match expr with
    Noexpr -> string_of_decl_kw kw ^ " " ^ string_of_typ t ^ " " ^ id ^ ";\n"
  | _ -> string_of_decl_kw kw ^ " " ^ string_of_typ t ^ " " ^ id ^ " = " ^
      string_of_expr expr ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^ String.concat "" (List.map string_of_stmt fdecl.body) ^ "}\n"

let string_of_program (globals, funcs) =
  String.concat "" (List.map string_of_global globals) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
