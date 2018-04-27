(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq
  | And | Or | MatMult | Mod | Exp

type uop = Neg | Not

type typ = Int | Bool | Float | String | Void | Exc | Array of typ
  | Matrix of typ | Func of typ list * typ | BuiltInFunc | Notyp

type decl_kw = Var | Create | Exception | Nokw | Auto

type expr =
  | Id of string
  | Int_Lit of int
  | Float_Lit of string
  | Bool_Lit of bool
  | String_Lit of string
  | Array_Lit of expr array
  | Empty_Array of typ * expr
  | Matrix_Lit of expr array array
  | Empty_Matrix of typ * expr * expr
  | Index_Expr of index_expr
  | Slice_Expr of slice_expr
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of expr * expr
  | Call of expr * expr list
  | One
  (* Used to indicate slice endpoint is +1 of previous endpoint *)
  | Slice_Inc
  (* Used to indicate slice endpoint is at the end *)
  | End
  | Noexpr

and islice = Index of expr | Slice of expr * expr

and index_expr = Sgl_Index of expr * islice | Dbl_Index of expr * islice * islice

and slice_expr = Sgl_Slice of expr * islice | Dbl_Slice of expr * islice * islice

type decl = decl_kw * typ * string * expr

type stmt =
  | Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Decl of decl
  | Try_Catch of try_catch
  | Protest of expr * expr

and try_catch = {
  try_block : stmt list;
  exc_type : string;
  exc_var : string;
  catch_block : stmt list;
}

type func_decl = {
    typ : typ;
    fname : string;
    formals : decl list;
    body : stmt list;
}

type program = decl list * func_decl list

let index_to_slice = function
  | Index i -> Slice(i, Slice_Inc)
  | _ -> raise (Failure "internal error: index_to_slice given a non-index")

let get_id_of_decl = function
  (_, _, s, _) -> s

let typ_of_container = function
  | Array t -> t
  | Matrix t -> t
  | _ -> raise (Failure "internal error: typ_of_container was given non-container")

let is_matrix = function
  | Matrix _ -> true
  | _ -> false

let is_array = function
  | Array _ -> true
  | _ -> false

let is_func = function
  | Func(_, _) | BuiltInFunc -> true
  | _ -> false

(* Pretty-printing functions *)

let string_of_op = function
  | Add -> "+"
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
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
  | Neg -> "-"
  | Not -> "!"

let string_of_decl_kw = function
  | Var -> "var"
  | Create -> "create"
  | Exception -> "exception"
  | Nokw -> ""
  | Auto -> "auto"

let rec string_of_typ = function
  | Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "string"
  | Void -> "void"
  | Exc -> "exc"
  | Array t -> "array<" ^ string_of_typ t ^ ">"
  | Matrix t -> "matrix<"  ^ string_of_typ t ^ ">"
  | Func(args, ret) -> "func<(" ^ String.concat ", " (List.map string_of_typ args) ^
      "):" ^ string_of_typ ret ^ ">"
  | BuiltInFunc -> "func<built-in>"
  | Notyp -> ""

(* Output should be valid output of string_of_typ *)
let rec typ_of_string s =
  match s with
  | "int" -> Int
  | "bool" -> Bool
  | "float" -> Float
  | "string" -> String
  | "void" -> Void
  | "exc" -> Exc
  | "func<built-in>" -> BuiltInFunc
  | "" -> Notyp
  | _ ->
      let array_regexp = Str.regexp "array<\\(.*\\)>" in
      let matrix_regexp = Str.regexp "matrix<\\(.*\\)>" in
      let func_regexp = Str.regexp "func<(\\(.*\\)):\\(.*\\)>" in
      if Str.string_match array_regexp s 0 then
        Array (typ_of_string (Str.matched_group 1 s))
      else if Str.string_match matrix_regexp s 0 then
        Matrix (typ_of_string (Str.matched_group 1 s))
      else if Str.string_match func_regexp s 0 then
        let args = Str.matched_group 1 s in
        let ret = Str.matched_group 2 s in
        let args = Str.split (Str.regexp ", ") args in
        Func(List.map typ_of_string args, typ_of_string ret)
      else raise (Failure ("internal error: typ_of_string given invalid type string " ^ s))

let rec string_of_expr expr =
  let string_of_islice = function
    | Index i -> string_of_expr i
    | Slice(i, j) -> string_of_expr i ^ ":" ^ string_of_expr j
  in

  let string_of_index_expr = function
    | Sgl_Index(e, i) -> string_of_expr e ^ "[" ^ string_of_islice i ^ "]"
    | Dbl_Index(e, i1, i2) ->
        string_of_expr e ^ "[" ^ string_of_islice i1 ^ ", " ^ string_of_islice i2 ^ "]"
  in

  let string_of_slice_expr = function
    | Sgl_Slice(e, s) -> string_of_expr e ^ "[" ^ string_of_islice s ^ "]"
    | Dbl_Slice(e, s1, s2) ->
        string_of_expr e ^ "[" ^ string_of_islice s1 ^ ", " ^ string_of_islice s2 ^ "]"
  in

  let string_of_array arr =
    "{|" ^ String.concat ", " (Array.to_list (Array.map string_of_expr arr)) ^ "|}"
  in

  let string_of_row row =
    "[" ^ String.concat ", " (Array.to_list (Array.map string_of_expr row)) ^ "]"
  in

  let string_of_matrix matrix =
    "[" ^ String.concat ", " (Array.to_list (Array.map string_of_row matrix)) ^ "]"
  in
  match expr with
  | Int_Lit l -> string_of_int l
  | Float_Lit l -> l
  | Bool_Lit true -> "True"
  | Bool_Lit false -> "False"
  | String_Lit l -> "\"" ^ l ^ "\""
  | Array_Lit l -> string_of_array l
  | Empty_Array(t, n) -> "{|type: " ^ string_of_typ t ^ ", size: " ^ string_of_expr n ^ "|}"
  | Matrix_Lit l -> string_of_matrix l
  | Empty_Matrix(t, r, c) -> "[type: " ^ string_of_typ t ^ ", dims: " ^ string_of_expr r ^ " x " ^
      string_of_expr c ^ "]"
  | Index_Expr e -> string_of_index_expr e
  | Slice_Expr e -> string_of_slice_expr e
  | Id s -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
  | Call(f, el) ->
      string_of_expr f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | One -> "[1]"
  | Slice_Inc -> "prev+1"
  | End -> "END"
  | Noexpr -> ""

let string_of_vdecl (kw, t, id, expr) =
  match kw, t, expr with
  | (Nokw, _, Noexpr) -> string_of_typ t ^ " " ^ id
  | (_, Exc, Noexpr) -> string_of_decl_kw kw ^ " " ^ id ^ ";\n"
  | (_, Notyp, Noexpr) -> string_of_decl_kw kw ^ " " ^ id ^ ";\n"
  | (_, _, Noexpr) -> string_of_decl_kw kw ^ " " ^ string_of_typ t ^ " " ^ id ^ ";\n"
  | (_, Notyp, _) -> string_of_decl_kw kw ^ " " ^ id ^ ";\n"
  | (_, _, _) -> string_of_decl_kw kw ^ " " ^ string_of_typ t ^ " " ^ id ^ " = " ^
      string_of_expr expr ^ ";\n"

let rec string_of_stmt = function
  | Block stmts ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr expr -> string_of_expr expr ^ ";\n"
  | Return expr ->
      if expr = Noexpr then "return;\n"
      else "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s, Block []) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | Decl decl -> string_of_vdecl decl
  | Try_Catch tc -> "try {\n" ^ String.concat "" (List.map string_of_stmt tc.try_block) ^
    "} catch " ^ tc.exc_type ^ "(" ^ tc.exc_var ^ ") {\n" ^
    String.concat "" (List.map string_of_stmt tc.catch_block) ^ "}\n"
  | Protest(t, e) -> "protest " ^ string_of_expr t ^ "(" ^ string_of_expr e ^ ");\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_vdecl fdecl.formals) ^
  ")\n{\n" ^ String.concat "" (List.map string_of_stmt fdecl.body) ^ "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
