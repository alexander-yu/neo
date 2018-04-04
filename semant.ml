(* Semantic checking for the Neo compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)
module TypeSet = Set.Make(struct type t = typ let compare = compare end)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

type symbol_table = {
  variables : typ StringMap.t;
  parent : symbol_table option
}

type translation_environment = {
  scope : symbol_table;
  array_types : TypeSet.t
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
      ("print", Void)
    ]
  in

  (* We wrap the program's main function call inside of another
   * true system main function; we rename the program's main function
   * as "prog_main", and create a system main with name "main". The system
   * main will also be responsible for initializing any global variables. *)
  let sys_main = "main" in
  let prog_main = "prog_main" in

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

  let rec type_of_identifier name scope =
    try StringMap.find name scope.variables
    with Not_found ->
      match scope.parent with
          None -> make_err ("undeclared identifier " ^ name)
        | Some parent -> type_of_identifier name parent
  in

  (* Return a semantically-checked expression *)
  let rec check_expr env expr =
    (* check all elements in container literal have valid type and have valid sizes *)
    let check_container_lit env e =
      let check_size e = match e with
          Array_Lit l ->
            if Array.length(l) > 0 then e
            else make_err "array must have non-zero size"
        | Matrix_Lit l ->
            if Array.length(l) > 0 && Array.length(l.(0)) > 0 then e
            else make_err "matrix must have non-zero dimensions"
        | _ -> make_err (string_of_expr e ^ " is not a supported container type")
      in

      (* Note: this returns a reversed list *)
      let check_equal_type t (env, checked) e =
        let env, (t', e') = check_expr env e in
        if t = t' then (env, (t', e') :: checked)
        else make_err ("container expected type " ^ string_of_typ t ^
          " but saw type " ^ string_of_typ t')
      in

      (* Note: this returns a reversed list of reversed lists *)
      let check_row_equal_type t (env, checked) row =
        let env, checked_row = Array.fold_left (check_equal_type t) (env, []) row in
        (env, checked_row :: checked)
      in

      let _ = check_size e in
      match e with
          Array_Lit l ->
            let env, sexpr = check_expr env l.(0) in
            let t = fst sexpr in
            let env, checked = Array.fold_left (check_equal_type t) (env, []) l in
            let checked = Array.of_list (List.rev checked) in
            (* Add array type to set *)
            let array_types = TypeSet.add (Array t) env.array_types in
            let env = { env with array_types } in
            (env, (Array t, SArray_Lit(checked)))
        | Matrix_Lit l ->
            let env, sexpr = check_expr env l.(0).(0) in
            let t = fst sexpr in
            let env, checked = Array.fold_left (check_row_equal_type t) (env, []) l in
            let checked = Array.of_list (List.rev checked) in
            let checked = Array.map List.rev checked in
            let checked = Array.map Array.of_list checked in
            (env, (Matrix t, SMatrix_Lit(checked)))
        | _ -> make_err "internal error: check_size should have rejected"
    in

    (* Raise an exception if the given rvalue type cannot be assigned to
      * the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else make_err err
    in

    match expr with
        Int_Lit  l -> (env, (Int, SInt_Lit l))
      | Float_Lit l -> (env, (Float, SFloat_Lit l))
      | Bool_Lit l -> (env, (Bool, SBool_Lit l))
      | Noexpr -> (env, (Void, SNoexpr))
      | Id s -> (env, (type_of_identifier s env.scope, SId s))
      | String_Lit s -> (env, (String, SString_Lit s))
      | Array_Lit _ as a ->
          check_container_lit env a
      | Empty_Array(t, n) ->
          let env, n' = check_expr env n in
          if fst n' != Int then make_err "size of empty array must be of type int"
          else (env, (Array t, SEmpty_Array(t, n')))
      | Matrix_Lit _ as m -> check_container_lit env m
      | Empty_Matrix(t, r, c) ->
          let env, r' = check_expr env r in
          let env, c' = check_expr env c in
          if fst r' != Int || fst c' != Int then make_err "dimensions of empty matrix must be of type int"
          else (env, (Matrix t, SEmpty_Matrix(t, r', c')))
      | Assign(e1, e2) -> (* TODO: check e1 for index/string *)
          (
            match e1 with
                Id s ->
                  let lt = type_of_identifier s env.scope
                  and env, (rt, e') = check_expr env e2 in
                  let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                    string_of_typ rt ^ " in " ^ string_of_expr expr
                  in
                  (* env not needed here; it won't be modified since
                   * call to print isn't a valid assignee *)
                  let _, sexpr = check_expr env e1 in
                  (env, (check_assign lt rt err, SAssign(sexpr, (rt, e'))))
              | _ -> make_err "not supported yet in assign"
          )
      | Unop(op, e) ->
          let env, (t, e') = check_expr env e in
          let ty = match op with
              Neg when t = Int || t = Float -> t
            | Not when t = Bool -> Bool
            | _ -> make_err ("illegal unary operator " ^
                string_of_uop op ^ string_of_typ t ^
                " in " ^ string_of_expr expr)
          in
          (env, (ty, SUnop(op, (t, e'))))
      | Binop(e1, op, e2) ->
          let env, (t1, e1') = check_expr env e1 in
          let env, (t2, e2') = check_expr env e2 in
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
          (env, (ty, SBinop((t1, e1'), op, (t2, e2'))))
        | Call("print", args) ->
            if List.length args != 1 then make_err "expecting 1 argument in print"
            else
              let env, arg = List.hd (List.map (check_expr env) args) in
              let ty = fst arg in
              (
                match ty with
                    Int | Bool | String | Matrix _ | Array _ ->
                      (env, (ty, SCall("print", [arg])))
                  | _ -> make_err ("type " ^ string_of_typ ty ^ " not supported by print")
              )
        | Call(fname, args) as call ->
            let typ = type_of_identifier fname env.scope in
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
                (* We can ignore env; array types will be
                 * found when processing the function formals *)
                let _, (expr_type, arg') = check_expr env arg_expr in
                let err = "illegal argument found " ^ string_of_typ expr_type ^
                  " expected " ^ string_of_typ arg_type ^ " in " ^
                  string_of_expr arg_expr
                in
                (check_assign arg_type expr_type err, arg')
              in
              let args' = List.map2 check_arg formals args in
              (env, (return_type, SCall(fname, args')))
        | _ -> make_err "not supported yet in check_expr"
  in

  (* Return semantically checked declaration *)
  let check_v_decl (env, checked) decl =
    let check_v_decl_type t =
      let err = "variable cannot have type " ^ string_of_typ t in
      match t with
          Void | Func(_, _) -> make_err err
        | _ -> ()
    in
    let add_v_decl scope decl =
      let _, t, s, _ = decl in
      add_decl scope s t
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
    let kw_err = "cannot declare type " ^
      string_of_typ t ^ " with keyword " ^ string_of_decl_kw kw
    in
    let _ = if kw != expr_kw then make_err kw_err in

    (* Check initialization type is valid *)
    let env, sexpr = check_expr env expr in
    let et = fst sexpr in
    let typ_err = "declared type " ^ string_of_typ t ^
      " but initialized with type " ^ string_of_typ et
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
    ({ scope ; array_types }, (kw, t, s, (t, snd sexpr)) :: checked)
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
      add_decl scope f.fname typ
    in
    let check_formal_type formal =
      let _, t, _, _ = formal in
      let err = "function argument cannot have type " ^ string_of_typ t in
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
                  let env, s', ret = check_stmt { env with scope } s in
                  (env, [s'], ret)
              | Return _ :: _ -> make_err "nothing may follow a return"
              | s :: ss ->
                  let env, s', ret1 = check_stmt env s in
                  let env, ss', ret2 = check_stmt_list env ss in
                  (env, s' :: ss', ret1 || ret2)
              | [] -> (env, [], false)
            in
            let env, sl', ret = check_stmt_list env sl in
            (* Return to parent scope *)
            ({ env with scope = parent_scope }, SBlock sl', ret)
        | _ -> make_err "not supported yet in check_stmt"
    in

    (* Rename main function to prog_main *)
    let fname = if func.fname = sys_main then prog_main else func.fname in
    let func = { func with fname } in

    (* Add function decl to parent scope *)
    let parent_scope = env.scope in
    let parent_scope = add_func_decl parent_scope func in

    (* Check formals have valid type *)
    let _ = List.iter check_formal_type func.formals in

    (* Build local symbol table of variables for this function *)
    let scope = { variables = StringMap.empty; parent = Some parent_scope } in
    let env = { env with scope } in
    let env, formals' = List.fold_left check_v_decl (env, []) func.formals in

    (* Check body *)
    let env, body =
      match check_stmt env (Block func.body) with
          env, SBlock sl, true -> (env, sl)
        | env, SBlock sl, false ->
            let err = "function has return type " ^ string_of_typ func.typ
              ^ " but no return statement found"
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

  let global_scope = { variables = built_in_decls; parent = None } in
  let env = { scope = global_scope; array_types = TypeSet.empty } in
  let env, globals' = List.fold_left check_v_decl (env, []) globals in
  let env, functions' = List.fold_left check_func_decl (env, []) functions in

  (* Ensure "main" is defined *)
  let _ =
    let main_err = "must have a main function of type func<():void>" in
    try
      let typ = type_of_identifier prog_main env.scope in
      match typ with
          Func([], Void) -> typ
        | _ -> make_err main_err
    with Not_found -> make_err main_err
  in

  let program = (List.rev globals', List.rev functions') in
  (env.array_types, program)
