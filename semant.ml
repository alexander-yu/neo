(* Semantic checking for the Neo compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)
module TypeSet = Set.Make(struct type t = typ let compare = compare end)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

type symbol_table = {
  (* The bool value represents whether or not the variable
   * has been assigned a value *)
  variables : (typ * bool) StringMap.t;
  parent : symbol_table option
}

type translation_environment = {
  scope : symbol_table;
  array_types : TypeSet.t;
}

let make_err err = raise (Failure err)

let check (globals, functions) =
  (* add built-in functions *)
  let built_in_funcs =
    let add_built_in_func map (name, typ) =
      let f_type = Func([], typ) in
      StringMap.add name (f_type, true) map
    in
    List.fold_left add_built_in_func StringMap.empty [
      ("print", Void);
      ("free", Void)
    ]
  in

  (* We wrap the program's main function call inside of another
   * true system main function; we rename the program's main function
   * as "prog_main", and create a system main with name "main". The system
   * main will also be responsible for initializing any global variables. *)
  let sys_main = "main" in
  let prog_main = "prog_main" in

  let add_decl scope name typ has_value =
    let built_in_err = "identifier " ^ name ^ " may not be defined" in
    let dup_err = "duplicate identifier " ^ name in

    (* Cannot redefine built-ins *)
    let is_built_in = StringMap.mem name built_in_funcs in
    let _ = if is_built_in then make_err built_in_err in

    (* Cannot declare duplicate in same scope *)
    let is_dup = StringMap.mem name scope.variables in
    let _ = if is_dup then make_err dup_err in
    { scope with variables = StringMap.add name (typ, has_value) scope.variables }
  in

  let rec lookup name scope =
    try StringMap.find name scope.variables
    with Not_found ->
      match scope.parent with
          None -> make_err ("undeclared identifier " ^ name)
        | Some parent -> lookup name parent
  in

  (* Return a semantically-checked expression *)
  let rec check_expr env expr =
    (* check all elements in container literal have valid type and have valid sizes *)
    let check_container_lit env e =
      let check_size e = match e with
          Array_Lit l ->
            if Array.length(l) > 0 then e
            else make_err ("array has zero length in " ^ string_of_expr e)
        | Matrix_Lit l ->
            if Array.length(l) > 0 && Array.length(l.(0)) > 0 then e
            else make_err ("matrix has a dimension of size 0 in " ^ string_of_expr e)
        | _ -> make_err "internal error: check_container_lit passed non-container type"
      in

      (* Note: this returns a reversed list *)
      let check_equal_type t (env, checked) e =
        let env, (t', e') = check_expr env e in
        if t = t' then (env, (t', e') :: checked)
        else make_err ("container expected type " ^ string_of_typ t ^
          " but saw type " ^ string_of_typ t' ^ " in " ^ string_of_expr e)
      in

      (* Note: this returns a reversed list of reversed lists *)
      let check_row_equal_type t (env, checked) row =
        let env, checked_row = Array.fold_left (check_equal_type t) (env, []) row in
        (env, checked_row :: checked)
      in

      let _ = check_size e in
      match e with
          Array_Lit l ->
            let env, (t, _) = check_expr env l.(0) in
            let env, checked = Array.fold_left (check_equal_type t) (env, []) l in
            let checked = Array.of_list (List.rev checked) in
            (* Add array type to set *)
            let array_types = TypeSet.add (Array t) env.array_types in
            let env = { env with array_types } in
            (env, (Array t, SArray_Lit(checked)))
        | Matrix_Lit l ->
            let env, (t, _) = check_expr env l.(0).(0) in
            let env, checked = Array.fold_left (check_row_equal_type t) (env, []) l in
            let checked = Array.of_list (List.rev checked) in
            let checked = Array.map List.rev checked in
            let checked = Array.map Array.of_list checked in
            (env, (Matrix t, SMatrix_Lit(checked)))
        | _ -> make_err "internal error: check_container_lit passed non-container type"
    in

    (* Raise an exception if the given rvalue type cannot be assigned to
      * the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else make_err err
    in

    let get_idx_err e =
      "index " ^ string_of_expr e ^ " is not an int"
    in

    let check_islice env = function
        Index i ->
          let env, (t, i') = check_expr env i in
          if t = Int then (env, SIndex((t, i'))) else make_err (get_idx_err i)
      | Slice(i, j) ->
          let env, (ti, i') = check_expr env i in
          let env, (tj, j') =
            match j with
                Slice_Inc -> (env, (Int, SSlice_Inc))
              | _ -> check_expr env j
          in
          let _ = if ti != Int then make_err (get_idx_err i) in
          let _ = if tj != Int then make_err (get_idx_err j) in
          (env, SSlice((ti, i'), (tj, j')))
    in

    let check_index_expr env i =
      match i with
          Sgl_Index(e, i) ->
            let env, (t, e') = check_expr env e in
            (
              match t with
                  Array _ ->
                    let env, si = check_islice env i in
                    let sindex = SIndex_Expr(SSgl_Index((t, e'), si)) in
                    (* Strip the container type *)
                    (env, (typ_of_container t, sindex))
                | Matrix _ ->
                    (* m[i] is semantically equivalent to m[i:i+1][:] *)
                    let env, ss1 = check_islice env (index_to_slice i) in
                    let ss2 = SSlice((Int, SInt_Lit 0), (Int, SEnd)) in
                    let sslice = SSlice_Expr(SDbl_Slice((t, e'), ss1, ss2)) in
                    (env, (t, sslice))
                | _ -> make_err ("indexed expression " ^ string_of_expr e ^
                    " is not an array or matrix")
            )
        | Dbl_Index(e, i1, i2) ->
            let env, (t, e') = check_expr env e in
            let _ =
              match t with
                  Matrix _ -> ()
                | _ -> make_err ("doubled-indexed expression " ^ string_of_expr e ^
                    " is not a matrix")
            in
            let env, si1 = check_islice env i1 in
            let env, si2 = check_islice env i2 in
            let sindex = SIndex_Expr(SDbl_Index((t, e'), si1, si2)) in
            (* Strip the container type *)
            (env, (typ_of_container t, sindex))
    in

    let check_slice_expr env i =
      match i with
          Sgl_Slice(e, s) ->
            let env, (t, e') = check_expr env e in
            (
              match t with
                  Array _ ->
                    let env, ss = check_islice env s in
                    let sslice = SSlice_Expr(SSgl_Slice((t, e'), ss)) in
                    (env, (t, sslice))
                | Matrix _ ->
                    (* m[i:j] is semantically equivalent to m[i:j][:] *)
                    let env, ss1 = check_islice env s in
                    let ss2 = SSlice((Int, SInt_Lit 0), (Int, SEnd)) in
                    let sslice = SSlice_Expr(SDbl_Slice((t, e'), ss1, ss2)) in
                    (env, (t, sslice))
                | _ -> make_err ("sliced expression " ^ string_of_expr e ^
                    " is not an array")
            )
        | Dbl_Slice(e, s1, s2) ->
            let env, (t, e') = check_expr env e in
            let _ =
              match t with
                  Matrix _ -> ()
                | _ -> make_err ("doubled-sliced expression " ^ string_of_expr e ^
                    " is not a matrix")
            in
            let env, ss1 = check_islice env s1 in
            let env, ss2 = check_islice env s2 in
            let sslice = SSlice_Expr(SDbl_Slice((t, e'), ss1, ss2)) in
            (env, (t, sslice))
    in
    let expr_s = string_of_expr expr in
    match expr with
        Int_Lit l -> (env, (Int, SInt_Lit l))
      | Float_Lit l -> (env, (Float, SFloat_Lit l))
      | Bool_Lit l -> (env, (Bool, SBool_Lit l))
      | Noexpr -> (env, (Void, SNoexpr))
      | Id s ->
          (* If we're here, this should mean that the expression is attempting
           * to access/read the symbol; thus, it needs to have a value *)
          let t, has_value = lookup s env.scope in
          let _ =
            if not has_value
            then make_err ("variable " ^ s ^ " must be initialized before use")
          in
          (env, (t, SId s))
      | String_Lit s -> (env, (String, SString_Lit s))
      | Array_Lit _ as a -> check_container_lit env a
      | Empty_Array(t, n) ->
          let env, (nt, n') = check_expr env n in
          if nt != Int then make_err ("non-integer length specified in " ^ expr_s)
          else (env, (Array t, SEmpty_Array(t, (nt, n'))))
      | Matrix_Lit _ as m -> check_container_lit env m
      | Empty_Matrix(t, r, c) ->
          let env, (rt, r') = check_expr env r in
          let env, (ct, c') = check_expr env c in
          if rt != Int || ct != Int
          then make_err ("non-integer dimensions specified in " ^ expr_s)
          else (env, (Matrix t, SEmpty_Matrix(t, (rt, r'), (ct, c'))))
      | Index_Expr i -> check_index_expr env i
      | Slice_Expr s -> check_slice_expr env s
      | Assign(e1, e2) ->
          (
            match e1 with
                Id s ->
                  (* Set the value flag for a symbol; here, since we're assigning a value,
                   * we need to make sure the flag is now true *)
                  let rec set_value_flag s scope =
                    try
                      let typ, _ = StringMap.find s scope.variables in
                      let variables = StringMap.add s (typ, true) scope.variables in
                      { scope with variables }
                    with Not_found ->
                      (* If not found in current scope, search its parent and return the
                       * updated parent *)
                       match scope.parent with
                          None ->
                            make_err
                            ("internal error: lookup should have " ^
                            "thrown unidentified error")
                        | Some parent ->
                            let parent = set_value_flag s parent in
                            { scope with parent = Some parent }
                  in
                  let lt, _ = lookup s env.scope in
                  let env = { env with scope = set_value_flag s env.scope} in
                  let env, (rt, e') = check_expr env e2 in
                  let err =
                    "illegal assignment " ^ string_of_typ lt ^ " = " ^
                    string_of_typ rt ^ " in " ^ expr_s
                  in
                  (env, (check_assign lt rt err, SAssign((lt, SId s), (rt, e'))))
              | Index_Expr i ->
                  let env, (lt, i') = check_index_expr env i in
                  let env, (rt, e') = check_expr env e2 in
                  let err =
                    "illegal assignment " ^ string_of_typ lt ^ " = " ^
                    string_of_typ rt ^ " in " ^ expr_s
                  in
                  (env, (check_assign lt rt err, SAssign((lt, i'), (rt, e'))))
              | Slice_Expr s ->
                  let env, (lt, s') = check_slice_expr env s in
                  let env, (rt, e') = check_expr env e2 in
                  let err =
                    "illegal assignment " ^ string_of_typ lt ^ " = " ^
                    string_of_typ rt ^ " in " ^ expr_s
                  in
                  (env, (check_assign lt rt err, SAssign((lt, s'), (rt, e'))))
              | _ -> make_err (expr_s ^ " is not assignable")
          )
      | Unop(op, e) ->
          let env, (t, e') = check_expr env e in
          let ty = match op with
              Neg when t = Int || t = Float -> t
            | Not when t = Bool -> Bool
            | _ -> make_err ("illegal unary operator " ^
                string_of_uop op ^ string_of_typ t ^
                " in " ^ expr_s)
          in
          (env, (ty, SUnop(op, (t, e'))))
      | Binop(e1, op, e2) ->
          let env, (t1, e1') = check_expr env e1 in
          let env, (t2, e2') = match e2 with
              One ->
                let rec get_one typ = match typ with
                    Int -> Int_Lit 1
                  | Float -> Float_Lit "1."
                  | Matrix t -> get_one t
                  | _ -> make_err ("type " ^ string_of_typ typ ^
                      " cannot be incremented/decremented")
                in
                let one = get_one t1 in
                check_expr env one
            | _ -> check_expr env e2
          in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
              Add | Sub | Mult | Div     when same && t1 = Int   -> Int
            | Add | Sub | Mult | Div     when same && t1 = Float -> Float
            | Equal | Neq                when same               -> Bool
            | Less | Leq | Greater | Geq when same && (t1 = Int || t1 = Float) -> Bool
            | And | Or                   when same && t1 = Bool -> Bool
            | Mod | Exp                  when same && t1 = Int   -> Int
            | Mod | Exp                  when same && t1 = Float -> Float
            | MatMult                     -> make_err "not supported yet in check_expr"
            | _ -> make_err ("illegal binary operator " ^
                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                string_of_typ t2 ^ " in " ^ expr_s)
          in
          (env, (ty, SBinop((t1, e1'), op, (t2, e2'))))
        | Call("print", args) ->
            if List.length args != 1 then make_err ("expecting 1 argument in " ^ expr_s)
            else
              let env, (t, arg) = List.hd (List.map (check_expr env) args) in
              (
                match t with
                    Int | Float | Bool | String | Matrix _ | Array _ ->
                      (env, (t, SCall("print", [(t, arg)])))
                  | _ -> make_err ("unsupported print parameter type in " ^ expr_s)
              )
        | Call("free", args) ->
            if List.length args != 1 then make_err ("expecting 1 argument in " ^ expr_s)
            else
              let env, (t, arg) = List.hd (List.map (check_expr env) args) in
              (
                match t with
                    Matrix _ | Array _ ->
                      (env, (t, SCall("free", [(t, arg)])))
                  | _ -> make_err ("unsupported free parameter type in " ^ expr_s)
              )
        | Call(fname, args) ->
            let typ, _ = lookup fname env.scope in
            let formals, return_type = match typ with
                Func(formals, return_type) -> (formals, return_type)
              | _ -> make_err (fname ^ " is not a function in " ^ expr_s)
            in
            let param_length = List.length formals in
            if List.length args != param_length then
              make_err ("expecting " ^ string_of_int param_length ^
                " arguments in " ^ expr_s)
            else
              let check_arg (env, checked) arg_type arg_expr =
                let env, (expr_type, arg') = check_expr env arg_expr in
                let err =
                  "illegal argument " ^ string_of_expr arg_expr ^
                  ", found " ^ string_of_typ expr_type ^
                  ", expected " ^ string_of_typ arg_type ^
                  " in " ^ expr_s
                in
                let checked_arg = (check_assign arg_type expr_type err, arg') in
                (env, checked_arg :: checked)
              in
              let env, args' = List.fold_left2 check_arg (env, []) formals args in
              (env, (return_type, SCall(fname, List.rev args')))
        | One -> make_err "internal error: One should not be passed to check_expr"
        | Slice_Inc -> make_err "internal error: Slice_Inc should not be passed to check_expr"
        | End -> (env, (Int, SEnd))
  in

  (* Return semantically checked declaration *)
  let check_v_decl override_true (env, checked) decl =
    let check_v_decl_type t =
      let err =
        "illegal declaration type " ^ string_of_typ t ^
        " in " ^ string_of_vdecl decl
      in
      match t with
          Void | Func(_, _) -> make_err err
        | _ -> ()
    in
    let add_v_decl scope decl =
      let _, t, s, expr = decl in
      (* If override_true, we override the initializer check with a value of true;
       * this is needed for globals (we'll be generating a value for them when
       * declared, and also because this behavior is impossible to track for
       * globals anyhow) as well as function parameters (because presumably
       * these parameters will themselves be checked during the actual function
       * call) *)
      add_decl scope s t (expr != Noexpr || override_true)
    in
    let kw, t, s, expr = decl in

    (* Check decl type is valid *)
    let _ = check_v_decl_type t in

    (* Check keyword matches type *)
    let expr_kw = match t with
        Int | Bool | Float | String -> Var
      | Exc -> Exception
      | Array _ | Matrix _ -> Create
      | _ -> make_err "internal error: check_v_decl_type should have rejected"
    in
    let kw_err =
      "illegal use of declaration keyword " ^ string_of_decl_kw kw ^
      " for type " ^ string_of_typ t ^ " in " ^ string_of_vdecl decl
    in
    let _ = if kw != expr_kw then make_err kw_err in

    (* Check initialization type is valid; note that an empty initialization
     * is valid, as this means we're declaring but not initializing *)
    let env, (et, expr') = check_expr env expr in
    let typ_err =
      "declared type " ^ string_of_typ t ^
      " but initialized with type " ^ string_of_typ et ^
      " in " ^ string_of_vdecl decl
    in
    let _ = if expr != Noexpr && t <> et then make_err typ_err in

    (* Add decl to scope *)
    let scope = add_v_decl env.scope decl in

    (* Add array type to set if we declared an array type *)
    let array_types = match t with
        Array _ -> TypeSet.add t env.array_types
      | _ -> env.array_types
    in

    (* After type check, we explicitly add decl's type to the sexpr,
     * to handle the case where we have a void initialization (i.e.
     * declaration but not initialization) *)
    ({ scope ; array_types }, (kw, t, s, (t, expr')) :: checked)
  in

  (* Return semantically checked function *)
  let check_func_decl (env, checked) func =
    let add_func_decl scope f =
      let typ_of_func f =
        let get_decl_type (_, typ, _, _) = typ in
        let arg_types = List.map get_decl_type f.formals in
        Func(arg_types, f.typ)
      in
      let typ = typ_of_func f in
      (* Note function declarations automatically have a value, since
       * their declarations include their definitions *)
      add_decl scope f.fname typ true
    in
    let check_formal_type fname formal =
      let _, t, _, _ = formal in
      let err =
        "illegal argument type " ^ string_of_typ t ^ " in " ^
        string_of_vdecl formal ^ " for the function " ^ fname
      in
      if t = Exc || t = Void then make_err err
    in
    (* Return a semantically-checked statement, along with a bool
     * indicating if there was at least one return statement
     * (somewhere in the statement itself *)
    let rec check_stmt env stmt =
      let check_bool_expr env e =
        let env, (t', e') = check_expr env e
        and err = "expected Boolean expression in " ^ string_of_expr e
        in
        if t' != Bool then make_err err else (env, (t', e'))
      in
      match stmt with
          Expr e ->
            let env, sexpr = check_expr env e in
            (env, SExpr(sexpr), false)
        | If(p, b1, b2) ->
            let env, p' = check_bool_expr env p in
            let env, b1', ret1 = check_stmt env b1 in
            let env, b2', ret2 = check_stmt env b2 in
            (env, SIf(p', b1', b2'), ret1 || ret2)
        | While(p, s) ->
            let env, p' = check_bool_expr env p in
            let env, s', ret = check_stmt env s in
            (env, SWhile(p', s'), ret)
        | Return e ->
            let env, (t, e') = check_expr env e in
            if t = func.typ then (env, SReturn(t, e'), true)
            else make_err ("return gives " ^ string_of_typ t ^ " expected " ^
              string_of_typ func.typ ^ " in " ^ string_of_expr e)
        (* A block is correct if each statement is correct and nothing
        * follows any return statement. Blocks define their own scope. *)
        | Block sl ->
            let parent_scope = env.scope in
            let scope = { variables = StringMap.empty; parent = Some parent_scope } in
            let rec check_stmt_list env = function
                [Return _ as s] ->
                  let env, s', ret = check_stmt env s in
                  (env, [s'], ret)
              | Return _ as r :: _ ->
                  make_err ("unreachable code after the return statement " ^ string_of_stmt r)
              | s :: ss ->
                  let env, s', ret1 = check_stmt env s in
                  let env, ss', ret2 = check_stmt_list env ss in
                  (env, s' :: ss', ret1 || ret2)
              | [] -> (env, [], false)
            in
            let env, sl', ret = check_stmt_list { env with scope } sl in
            (* Return to parent scope *)
            ({ env with scope = parent_scope }, SBlock sl', ret)
        | Decl d ->
            let env, checked = check_v_decl false (env, []) d in
            let sdecl = List.hd checked in
            (env, SDecl sdecl, false)
        | _ -> make_err "not supported yet in check_stmt"
    in

    (* Rename main function to prog_main *)
    let old_fname = func.fname in
    let fname = if old_fname = sys_main then prog_main else old_fname in
    let func = { func with fname } in

    (* Add function decl to parent scope *)
    let parent_scope = env.scope in
    let parent_scope = add_func_decl parent_scope func in

    (* Check formals have valid type *)
    let _ = List.iter (check_formal_type old_fname) func.formals in

    (* Build local symbol table of variables for this function *)
    let scope = { variables = StringMap.empty; parent = Some parent_scope } in
    let env = { env with scope } in
    (* Set override_true; function parameters should have values *)
    let env, formals' = List.fold_left (check_v_decl true) (env, []) func.formals in

    (* Check body *)
    let env, body =
      match check_stmt env (Block func.body) with
          env, SBlock sl, true -> (env, sl)
        | env, SBlock sl, false ->
            let err =
              "function " ^ old_fname ^ " has return type " ^
              string_of_typ func.typ ^ " but no return statement found"
            in
            if func.typ != Void then make_err err else (env, sl)
        | _ ->
            let err = "internal error: block didn't become a block?" in
            make_err err
    in
    (
      (* Return to parent scope *)
      { env with scope = parent_scope },
      (* body of check_func_decl *)
      { styp = func.typ;
        sfname = func.fname;
        sformals = formals';
        sbody = body;
      } :: checked
    )
  in

  let global_scope = { variables = built_in_funcs; parent = None } in
  let env = { scope = global_scope; array_types = TypeSet.empty; } in
  (* Set override_true; globals will have a default value generated at declaration *)
  let env, globals' = List.fold_left (check_v_decl true) (env, []) globals in
  let env, functions' = List.fold_left check_func_decl (env, []) functions in

  (* Ensure "main" is defined *)
  let _ =
    let main_err = "must have a main function of type func<():void>" in
    try
      let typ, _ = lookup prog_main env.scope in
      match typ with
          Func([], Void) -> typ
        | _ -> make_err main_err
    with Not_found -> make_err main_err
  in

  let program = (List.rev globals', List.rev functions') in
  (env.array_types, program)
