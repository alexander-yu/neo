(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SId of string
  | SInt_Lit of int
  | SFloat_Lit of string
  | SBool_Lit of bool
  | SMatrix_Lit of sexpr array array
  | SEmpty_Matrix_Lit of typ * sexpr * sexpr
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of sexpr * sexpr
  | SCall of string * sexpr list
  | SNoexpr

type sdecl = decl_kw * typ * string * sexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SDecl of sdecl

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : sdecl list;
    sbody : sstmt list;
}

type sprogram = sdecl list * sfunc_decl list

(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SId s -> s
  | SInt_Lit l -> string_of_int l
  | SBool_Lit true -> "True"
  | SBool_Lit false -> "False"
  | SFloat_Lit l -> l
  | SMatrix_Lit l -> string_of_smatrix l
  | SEmpty_Matrix_Lit(t, r, c) -> "[type: " ^ string_of_typ t ^ ", dims: " ^
      string_of_sexpr r ^ " x " ^ string_of_sexpr c ^ "]"
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(e1, e2) -> string_of_sexpr e1 ^ " = " ^ string_of_sexpr e2
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SNoexpr -> ""
          ) ^ ")"

and string_of_srow row =
  "[" ^ String.concat ", " (Array.to_list (Array.map string_of_sexpr row)) ^ "]"

and string_of_smatrix matrix =
  "[" ^ String.concat ", " (Array.to_list (Array.map string_of_srow matrix)) ^ "]"

and string_of_sstmt = function
    SBlock stmts ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr expr -> string_of_sexpr expr ^ ";\n";
  | SReturn expr -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock []) ->
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SDecl d -> string_of_svdecl d

and string_of_svdecl (kw, t, id, sexpr) = match t, sexpr with
    (Exc, (_, SNoexpr)) -> string_of_decl_kw kw ^ " " ^ id ^ ";\n"
    | (_, (_, SNoexpr)) -> string_of_decl_kw kw ^ " " ^ string_of_typ t ^ " " ^ id ^ ";\n"
    | (_, _) -> string_of_decl_kw kw ^ " " ^ string_of_typ t ^ " " ^ id ^ " = " ^
      string_of_sexpr sexpr ^ ";\n"

and string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map get_id_of_decl fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  String.concat "" (List.map string_of_svdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
