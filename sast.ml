(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SId of string
  | SInt_Lit of int
  | SFloat_Lit of string
  | SBool_Lit of bool
  | SString_Lit of string
  | SArray_Lit of sexpr array
  | SEmpty_Array of typ * sexpr
  | SMatrix_Lit of sexpr array array
  | SEmpty_Matrix of typ * sexpr * sexpr
  | SIndex_Expr of sindex_expr
  | SSlice_Expr of sslice_expr
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of sexpr * sexpr
  | SCall of sexpr * sexpr list
  | SSlice_Inc
  | SEnd
  | SNoexpr

and sislice = SIndex of sexpr | SSlice of sexpr * sexpr

and sindex_expr = SSgl_Index of sexpr * sislice | SDbl_Index of sexpr * sislice * sislice

and sslice_expr = SSgl_Slice of sexpr * sislice | SDbl_Slice of sexpr * sislice * sislice

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
  let string_of_sislice = function
      SIndex i -> string_of_sexpr i
    | SSlice(i, j) -> string_of_sexpr i ^ ":" ^ string_of_sexpr j
  in

  let string_of_sindex_expr = function
      SSgl_Index(e, i) -> string_of_sexpr e ^ "[" ^ string_of_sislice i ^ "]"
    | SDbl_Index(e, i1, i2) ->
        string_of_sexpr e ^ "[" ^ string_of_sislice i1 ^ ", " ^ string_of_sislice i2 ^ "]"
  in

  let string_of_sslice_expr = function
      SSgl_Slice(e, s) -> string_of_sexpr e ^ "[" ^ string_of_sislice s ^ "]"
    | SDbl_Slice(e, s1, s2) ->
        string_of_sexpr e ^ "[" ^ string_of_sislice s1 ^ ", " ^ string_of_sislice s2 ^ "]"
  in

  let string_of_sarray arr =
    "{|" ^ String.concat ", " (Array.to_list (Array.map string_of_sexpr arr)) ^ "|}"
  in

  let string_of_srow row =
    "[" ^ String.concat ", " (Array.to_list (Array.map string_of_sexpr row)) ^ "]"
  in

  let string_of_smatrix matrix =
    "[" ^ String.concat ", " (Array.to_list (Array.map string_of_srow matrix)) ^ "]"
  in
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SId s -> s
  | SInt_Lit l -> string_of_int l
  | SBool_Lit true -> "True"
  | SBool_Lit false -> "False"
  | SFloat_Lit l -> l
  | SString_Lit s -> s
  | SArray_Lit l -> string_of_sarray l
  | SEmpty_Array(t, n) -> "{|type: " ^ string_of_typ t ^ ", size: " ^
      string_of_sexpr n ^ "|}"
  | SMatrix_Lit l -> string_of_smatrix l
  | SEmpty_Matrix(t, r, c) -> "[type: " ^ string_of_typ t ^ ", dims: " ^
      string_of_sexpr r ^ " x " ^ string_of_sexpr c ^ "]"
  | SIndex_Expr e -> string_of_sindex_expr e
  | SSlice_Expr e -> string_of_sslice_expr e
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(e1, e2) -> string_of_sexpr e1 ^ " = " ^ string_of_sexpr e2
  | SCall(f, el) ->
      string_of_sexpr f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SSlice_Inc -> "prev+1"
  | SEnd -> "END"
  | SNoexpr -> ""
          ) ^ ")"

let string_of_svdecl (kw, t, id, sexpr) = match t, sexpr with
  (Exc, (_, SNoexpr)) -> string_of_decl_kw kw ^ " " ^ id ^ ";\n"
  | (_, (_, SNoexpr)) -> string_of_decl_kw kw ^ " " ^ string_of_typ t ^ " " ^ id ^ ";\n"
  | (_, _) -> string_of_decl_kw kw ^ " " ^ string_of_typ t ^ " " ^ id ^ " = " ^
    string_of_sexpr sexpr ^ ";\n"

let rec string_of_sstmt = function
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

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map get_id_of_decl fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  String.concat "" (List.map string_of_svdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
