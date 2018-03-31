(* Semantic checking for the Neo compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

type symbol_table = {
  variables : typ StringMap.t;
  parent : symbol_table option
}

let make_err err = raise (Failure err)

let check (globals, functions) =
  (* add built-in functions *)
  let built_in_decls =
    let add_built_in_decl map (name, typ) =
      let f_type = Func([], typ) in
      StringMap.add name f_type map
    in
    List.fold_left add_built_in_decl StringMap.empty [
      ("print", Void);
      ("printf", Void)
    ]
  in

  let add_decl scope name typ =
    let built_in_err = "identifier " ^ name ^ " may not be defined" in
    let dup_err = "duplicate identifier " ^ name in
    (* Cannot redefine built-ins *)
    let is_built_in = StringMap.mem name built_in_decls in
    let _ = if is_built_in then make_err built_in_err in
    (* Cannot declare duplicate in same scope *)
    let is_dup = StringMap.mem name scope.variables in
    let _ = if is_dup then make_err dup_err in
    { scope with variables = StringMap.add name typ scope.variables }
  in

  let add_func_decl scope f =
    let typ_of_func f =
      let get_decl_type (_, typ, _, _) = typ in
      let arg_types = List.map get_decl_type f.formals in
      Func(arg_types, f.typ)
    in
    let typ = typ_of_func f in
    add_decl scope f.fname typ
  in

  let add_v_decl scope decl =
    let _, t, s, _ = decl in
    add_decl scope s t
  in

  let rec type_of_identifier name scope =
    try StringMap.find name scope.variables
    with Not_found ->
      match scope.parent with
          None -> make_err ("undeclared identifier " ^ name)
        | Some parent -> type_of_identifier name parent
  in

  (* Return a semantically-checked expression *)
  let rec check_expr scope expr =
    (* check all elements in container literal have valid type and have valid sizes *)
    let check_container_lit scope e =
      let check_size e = match e with
          Matrix_Lit l ->
            if Array.length(l) > 0 && Array.length(l.(0)) > 0 then e
            else make_err (string_of_matrix l ^ " must have non-zero dimensions")
        | _ -> make_err (string_of_expr e ^ " is not a supported container type")
      in

      let check_equal_type t e =
        let t', e' = check_expr scope e in
        if t == t' then (t', e')
        else make_err ("container expected type " ^ string_of_typ t ^
          " but saw type " ^ string_of_typ t')
      in

      let _ = check_size e in
      match e with
          Matrix_Lit l ->
            let t = fst (check_expr scope l.(0).(0)) in
            (Matrix t, SMatrix_Lit(Array.map (Array.map (check_equal_type t)) l))
        | _ -> make_err "internal error: check_size should have rejected"
    in

    (* Raise an exception if the given rvalue type cannot be assigned to
      * the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else make_err err
    in

    match expr with
        Int_Lit  l -> (Int, SInt_Lit l)
      | Float_Lit l -> (Float, SFloat_Lit l)
      | Bool_Lit l  -> (Bool, SBool_Lit l)
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s scope, SId s)
      | Matrix_Lit _ as m -> check_container_lit scope m
      | Assign(e1, e2) -> (* TODO: check e1 for index/string *)
          (
            match e1 with
                Id s ->
                  let lt = type_of_identifier s scope
                  and (rt, e') = check_expr scope e2 in
                  let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                    string_of_typ rt ^ " in " ^ string_of_expr expr
                  in
                  (check_assign lt rt err, SAssign(check_expr scope e1, (rt, e')))
              | _ -> make_err "not supported yet in assign"
          )
      | Unop(op, e) ->
          let t, e' = check_expr scope e in
          let ty = match op with
              Neg when t = Int || t = Float -> t
            | Not when t = Bool -> Bool
            | _ -> make_err ("illegal unary operator " ^
                string_of_uop op ^ string_of_typ t ^
                " in " ^ string_of_expr expr)
          in
          (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) ->
          let t1, e1' = check_expr scope e1
          and t2, e2' = check_expr scope e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
              Add | Sub | Mult | Div     when same && t1 = Int   -> Int
            | Add | Sub | Mult | Div     when same && t1 = Float -> Float
            | Equal | Neq                when same               -> Bool
            | Less | Leq | Greater | Geq when same && (t1 = Int || t1 = Float) -> Bool
            | And | Or                   when same && t1 = Bool -> Bool
            | _ -> make_err ("illegal binary operator " ^
                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                string_of_typ t2 ^ " in " ^ string_of_expr expr)
          in
          (ty, SBinop((t1, e1'), op, (t2, e2')))
        | Call("print", args) ->
            if List.length args != 1 then make_err "expecting 1 argument in print"
            else
              let arg = List.hd (List.map (check_expr scope) args) in
              let ty = fst arg in
              (
                match ty with
                    Int | Bool | Matrix _ -> (ty, SCall("print", [arg]))
                  | _ -> make_err ("type " ^ string_of_typ ty ^ " not supported by print")
              )
        | Call(fname, args) as call ->
            let typ = type_of_identifier fname scope in
            let formals, return_type = match typ with
                Func(formals, return_type) -> (formals, return_type)
              | _ -> make_err (fname ^ " is not a function")
            in
            let param_length = List.length formals in
              if List.length args != param_length then
                make_err ("expecting " ^ string_of_int param_length ^
                  " arguments in " ^ string_of_expr call)
              else
                let check_arg arg_type arg_expr =
                let expr_type, arg' = check_expr scope arg_expr in
                let err = "illegal argument found " ^ string_of_typ expr_type ^
                  " expected " ^ string_of_typ arg_type ^ " in " ^
                  string_of_expr arg_expr
                in
                (check_assign arg_type expr_type err, arg')
            in
            let args' = List.map2 check_arg formals args in
            (return_type, SCall(fname, args'))
        | _ -> make_err "not supported yet in check_expr"
  in

  (* Return semantically checked declaration *)
  let check_decl scope decl =
    let check_decl_type t =
      let err = "variable cannot have type " ^ string_of_typ t in
      match t with
          Void | Func(_, _) -> make_err err
        | _ -> ()
    in
    let kw, t, s, expr = decl in

    (* Check decl type is valid *)
    let _ = check_decl_type t in

    (* Check keyword matches type *)
    let expr_kw = match t with
        Int | Bool | Float | String -> Var
      | Exc -> Exception
      | Array _ | Matrix _ -> Create
      | _ -> make_err "internal error: check_decl_type should have rejected"
    in
    let kw_err = "cannot declare type " ^
      string_of_typ t ^ " with keyword " ^ string_of_decl_kw kw
    in
    let _ = if kw != expr_kw then make_err kw_err in

    (* Check initialization type is valid *)
    let sexpr = check_expr scope expr in
    let et = fst sexpr in
    let typ_err = "declared type " ^ string_of_typ t ^
      " but initialized with type " ^ string_of_typ et
    in
    let _ = if expr != Noexpr && t != et then make_err typ_err in
    (kw, t, s, sexpr)
  in

  (* Return semantically checked function *)
  let check_function parent_scope func =
    let check_formal_type formal =
      let _, t, _, _ = formal in
      let err = "function argument cannot have type " ^ string_of_typ t in
      if t = Exc || t = Void then make_err err
    in
    let _ = List.iter check_formal_type func.formals in

    (* Build local symbol table of variables for this function *)
    let scope = { variables = StringMap.empty; parent = Some parent_scope } in
    let formals' = List.map (check_decl scope) func.formals in
    let scope = List.fold_left add_v_decl scope formals' in

    (* Return a semantically-checked statement, along with a bool
     * indicating if there was at least one return statement
     * (somewhere in the statement itself *)
    let rec check_stmt scope stmt =
      let check_bool_expr e =
        let t', e' = check_expr scope e
        and err = "expected Boolean expression in " ^ string_of_expr e
        in
        if t' != Bool then make_err err else (t', e')
      in
      match stmt with
          Expr e -> (SExpr(check_expr scope e), false)
        | If(p, b1, b2) ->
            let p' = check_bool_expr p in
            let b1', ret1 = check_stmt scope b1 in
            let b2', ret2 = check_stmt scope b2 in
            (SIf(p', b1', b2'), ret1 || ret2)
        | While(p, s) ->
            let p' = check_bool_expr p in
            let s', ret = check_stmt scope s in
            (SWhile(p', s'), ret)
        | Return e ->
            let t, e' = check_expr scope e in
            if t = func.typ then (SReturn(t, e'), true)
            else make_err ("return gives " ^ string_of_typ t ^ " expected " ^
              string_of_typ func.typ ^ " in " ^ string_of_expr e)
        (* A block is correct if each statement is correct and nothing
        * follows any return statement. Blocks define their own scope. *)
        | Block sl ->
            let scope = { variables = StringMap.empty; parent = Some scope } in
            let rec check_stmt_list = function
                [Return _ as s] ->
                  let s', ret = check_stmt scope s in
                  ([s'], ret)
              | Return _ :: _ -> make_err "nothing may follow a return"
              | s :: ss ->
                  let s', ret1 = check_stmt scope s in
                  let ss', ret2 = check_stmt_list ss in
                  (s' :: ss', ret1 || ret2)
              | [] -> ([], false)
            in
            let sl', ret = check_stmt_list sl in
            (SBlock sl', ret)
        | _ -> make_err "not supported yet in check_stmt"
    in
    (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = formals';
      sbody = match check_stmt scope (Block func.body) with
          SBlock sl, true -> sl
        | SBlock sl, false ->
            let err = "function has return type " ^ string_of_typ func.typ
              ^ " but no return statement found"
            in
            if func.typ != Void then make_err err else sl
        | _ ->
            let err = "internal error: block didn't become a block?" in
            make_err err
    }
  in

  (* Collect globals and function names into a global symbol table *)
  let global_scope = { variables = built_in_decls; parent = None } in
  let global_scope = List.fold_left add_v_decl global_scope globals in
  let global_scope = List.fold_left add_func_decl global_scope functions in

  (* Ensure "main" is defined *)
  let _ =
    let typ = type_of_identifier "main" global_scope in
    match typ with
        Func([], Void) -> typ
      | _ -> make_err "must have a main function of type func<():void>"
  in

  let globals' = List.map (check_decl global_scope) globals in
  let functions' = List.map (check_function global_scope) functions in
  (globals', functions')
