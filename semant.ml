(* Semantic checking for the Neo compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

type symbol_table = {
    variables : typ StringMap.t;
    parent : symbol_table option
  }

type translation_environment = {
    scope : symbol_table;
    (* Store any uses of native built-in functions; we'll be doing the same
     * with helper functions and declared functions. *)
    builtin_uses : StringSet.t;
    (* Store any uses of miscellaneous external helper functions,
     * such as get_matrix or set_matrix, which are not exposed as
     * built-in functions but are needed regardless for various operations *)
    helper_uses : StringSet.t;
    (* Store any uses of Neo functions (user-defined or standard library) *)
    func_uses : StringSet.t;
  }

let make_err err = raise (Failure err)

let helpers_of_native fname =
  let rec typ_contains_class typ typ_class =
    match typ, typ_class with
    | Matrix _, "matrix" -> true
    | Func(_, _), "function" -> true
    | Array _, "array" -> true
    | Array t, _ -> typ_contains_class t typ_class
    | _ -> string_of_typ typ = typ_class
  in
  let fname_fragments = Str.split (Str.regexp "_") fname in
  match fname_fragments with
  | ["print"; arg_type] | ["println"; arg_type] ->
      let arg_type = typ_of_string arg_type in
      (
        match arg_type with
        | Array t when typ_contains_class t "matrix" -> ["_print_array"; "_print_flat_matrix"]
        | Array t when typ_contains_class t "function" -> ["_print_array"; "_print_function"]
        | Array _ -> ["_print_array"]
        | _ -> []
      )
  (* Note we don't perform validation that we're being handed an array; validation on
  * argument types is already done elsewhere *)
  | ["deep"; "free"; _] -> ["_deep_free_array"]
  | [("insert" as fname); arg_type] | [("append" as fname); arg_type] ->
      let arg_type = typ_of_string arg_type in
      if is_array arg_type then ["_" ^ fname ^ "_array"] else []
  | _ -> []

let check (globals, functions) =
  (* add built-in functions *)
  let built_in_funcs =
    let add_built_in_func map name =
      StringMap.add name BuiltInFunc map
    in
    List.fold_left add_built_in_func StringMap.empty [
      "print";
      "println";
      "deep_free";
      "free";
      "length";
      "rows";
      "cols";
      "to_int";
      "to_float";
      "insert";
      "delete";
      "append";
      "die";
    ]
  in

  (* We wrap the program's main function call inside of another
   * true system main function; we rename the program's main function
   * as "prog_main", and create a system main with name "main". The system
   * main will also be responsible for initializing any global variables. *)
  let sys_main = "main" in
  let prog_main = "prog_main" in

  (* Check if a built-in function can be restricted to a given type *)
  let check_builtin_restrict typ err fname =
    (* First check typ is actually a function type *)
    let arg_types, ret_type =
      match typ with
      | Func(arg_types, ret_type) -> (arg_types, ret_type)
      | _ -> make_err err
    in
    let n_args = List.length arg_types in
    match fname with
    | "print" | "println" ->
        if n_args = 1 && ret_type = Void then ()
        else make_err err
    | "deep_free" ->
        let check_types arg_types ret_type =
          is_array (List.hd arg_types) && ret_type = Void
        in
        if n_args = 1 && check_types arg_types ret_type then ()
        else make_err err
    | "free" ->
        let check_types arg_types ret_type =
          let valid_args =
            is_array (List.hd arg_types) ||
            is_matrix (List.hd arg_types)
          in
          valid_args && ret_type = Void
        in
        if n_args = 1 && check_types arg_types ret_type then ()
        else make_err err
    | "length" ->
        let check_types arg_types ret_type =
          is_array (List.hd arg_types) && ret_type = Int
        in
        if n_args = 1 && check_types arg_types ret_type then ()
        else make_err err
    | "rows" | "cols" ->
        let check_types arg_types ret_type =
          is_matrix (List.hd arg_types) && ret_type = Int
        in
        if n_args = 1 && check_types arg_types ret_type then ()
        else make_err err
    | "to_int" ->
        let check_types arg_types ret_type =
          (arg_types = [Float] && ret_type = Int) ||
          (arg_types = [Matrix Float] && ret_type = Matrix Int)
        in
        if n_args = 1 && check_types arg_types ret_type then ()
        else make_err err
    | "to_float" ->
        let check_types arg_types ret_type =
          (arg_types = [Int] && ret_type = Float) ||
          (arg_types = [Matrix Int] && ret_type = Matrix Float)
        in
        if n_args = 1 && check_types arg_types ret_type then ()
        else make_err err
    | "insert" ->
        let check_types arg_types ret_type =
          let cont_type = List.hd arg_types in
          let valid_args =
            match cont_type with
            | Array _ ->
                arg_types = [cont_type; Int; typ_of_container cont_type]
            | Matrix _ ->
                arg_types = [cont_type; Int; cont_type]
            | _ -> false
          in
          valid_args && ret_type = cont_type
        in
        if n_args = 3 && check_types arg_types ret_type then ()
        else make_err err
    | "delete" ->
        let check_types arg_types ret_type =
          let cont_type = List.hd arg_types in
          let valid_args =
            match cont_type with
            | Array _ | Matrix _ -> arg_types = [cont_type; Int]
            | _ -> false
          in
          valid_args && ret_type = cont_type
        in
        if n_args = 2 && check_types arg_types ret_type then ()
        else make_err err
    | "append" ->
        let check_types arg_types ret_type =
          let cont_type = List.hd arg_types in
          let valid_args =
            match cont_type with
            | Array _ ->
                arg_types = [cont_type; typ_of_container cont_type]
            | Matrix _ ->
                arg_types = [cont_type; cont_type]
            | _ -> false
          in
          valid_args && ret_type = cont_type
        in
        if n_args = 2 && check_types arg_types ret_type then ()
        else make_err err
    | "die" ->
        let check_types arg_types ret_type =
          arg_types = [String] && ret_type = Void
        in
        if n_args = 1 && check_types arg_types ret_type then ()
        else make_err err
    | _ -> make_err err
  in

  (* Adds uses of helper functions to environment *)
  let add_helper_use env fname =
    { env with helper_uses = StringSet.add fname env.helper_uses }
  in

  let get_native_of_builtin env fname arg_types =
    (* Our built-in functions are written in a way such that
     * the first argument is all the information required to determine
     * which actual native function to call *)
    let type_tag = string_of_typ (List.hd arg_types) in
    let native_fname = "_" ^ fname ^ "_" ^ type_tag in
    let env =
      if (fname = "print" || fname = "println") && List.hd arg_types = BuiltInFunc then
        { env with builtin_uses = StringSet.add "_print_string" env.builtin_uses }
      else
        let env =
          { env with builtin_uses = StringSet.add native_fname env.builtin_uses }
        in
        let helpers = helpers_of_native native_fname in
        List.fold_left add_helper_use env helpers
    in
    (env, native_fname)
  in

  (* Raise an exception if the given rvalue type cannot be assigned to
  * the given lvalue type *)
  let check_assign env lvaluet (rvaluet, rexpr) err =
    if lvaluet = rvaluet then (env, (rvaluet, rexpr))
    (* Alternatively, there's a built-in reference; see if we can restrict
     * the built-in type to the assigned type *)
    else
      let is_builtin_assign lvaluet rvaluet =
        match lvaluet, rvaluet with
        | Func(_, _), BuiltInFunc -> true
        | _ -> false
      in
      if is_builtin_assign lvaluet rvaluet then
        (* In this case, rexpr must be of the form SId, since it's a
         * direct reference to a built-in *)
        match rexpr with
        | SId fname ->
            let _ = check_builtin_restrict lvaluet err fname in
            (* Replace built-in function with the underlying native function *)
            let arg_types =
              match lvaluet with
              | Func(arg_types, _) -> arg_types
              | _ ->
                  make_err ("internal error: " ^ string_of_typ lvaluet ^
                  " = " ^ string_of_typ rvaluet ^ " should have been rejected" ^
                  " by check_builtin_restrict")
            in
            let env, native_fname = get_native_of_builtin env fname arg_types in
            (env, (lvaluet, SId native_fname))
        | _ -> make_err ("internal error: only direct references to " ^
            "built-in functions should have type BuiltInFunc")
      else make_err err
  in

  let rec lookup scope name =
    try StringMap.find name scope.variables with
    | Not_found ->
        match scope.parent with
        | None -> make_err ("undeclared identifier " ^ name)
        | Some parent -> lookup parent name
  in

  (* Return a semantically-checked expression *)
  let rec check_expr env expr =
    (* check all elements in container literal have valid type and have valid sizes *)
    let check_container_lit env e =
      let expr_s = string_of_expr e in
      let check_size e =
        match e with
        | Array_Lit l ->
            if Array.length(l) > 0 then e
            else make_err ("array has zero length in " ^ expr_s)
        | Matrix_Lit l ->
            if Array.length(l) > 0 && Array.length(l.(0)) > 0 then e
            else make_err ("matrix has a dimension of size 0 in " ^ expr_s)
        | _ -> make_err "internal error: check_container_lit passed non-container type"
      in

      (* Note: this returns a reversed list *)
      let check_equal_type t (env, checked) e =
        let env, (t', e') = check_expr env e in
        let err =
          "container expected type " ^ string_of_typ t ^
          " but saw type " ^ string_of_typ t' ^ " in " ^ expr_s
        in
        let env, (t', e') = check_assign env t (t', e') err in
        (env, (t', e') :: checked)
      in

      (* Note: this returns a reversed list of reversed lists *)
      let check_row_equal_type t (env, checked) row =
        let env, checked_row = Array.fold_left (check_equal_type t) (env, []) row in
        (env, checked_row :: checked)
      in

      let _ = check_size e in
      match e with
      | Array_Lit l ->
          (* Get type of first element that isn't BuiltInFunc (i.e. explicitly typed) *)
          let rec find_typed l n =
            let _, (t, _) = check_expr env l.(n) in
            if t <> BuiltInFunc then t
            else if n + 1 < Array.length l then find_typed l (n + 1)
            else make_err ("array containing only built-in functions in " ^ expr_s)
          in
          let t = find_typed l 0 in
          let env, checked = Array.fold_left (check_equal_type t) (env, []) l in
          let checked = Array.of_list (List.rev checked) in
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

    let get_idx_err e =
      "index " ^ string_of_expr e ^ " is not an int"
    in

    let check_islice env = function
      | Index i ->
          let env, (t, i') = check_expr env i in
          if t = Int then (env, SIndex((t, i'))) else make_err (get_idx_err i)
      | Slice(i, j) ->
          let env, (ti, i') = check_expr env i in
          let env, (tj, j') =
            match j with
            | Slice_Inc -> (env, (Int, SSlice_Inc))
            | _ -> check_expr env j
          in
          let _ = if ti <> Int then make_err (get_idx_err i) in
          let _ = if tj <> Int then make_err (get_idx_err j) in
          (env, SSlice((ti, i'), (tj, j')))
    in

    let check_index_expr env i is_assign =
      match i with
      | Sgl_Index(e, i) ->
          let env =
            if is_assign then add_helper_use env "_set_array"
            else add_helper_use env "_get_array"
          in
          let env, (t, e') = check_expr env e in
          (
            match t with
            | Array _ ->
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
          let env =
            if is_assign then add_helper_use env "_set_matrix"
            else add_helper_use env "_get_matrix"
          in
          let env, (t, e') = check_expr env e in
          let _ =
            match t with
            | Matrix _ -> ()
            | _ -> make_err ("doubled-indexed expression " ^ string_of_expr e ^
                " is not a matrix")
          in
          let env, si1 = check_islice env i1 in
          let env, si2 = check_islice env i2 in
          let sindex = SIndex_Expr(SDbl_Index((t, e'), si1, si2)) in
          (* Strip the container type *)
          (env, (typ_of_container t, sindex))
    in

    let check_slice_expr env i is_assign =
      match i with
      | Sgl_Slice(e, s) ->
          let env =
            if is_assign then add_helper_use env "_set_slice_array"
            else add_helper_use env "_slice_array"
          in
          let env, (t, e') = check_expr env e in
          (
            match t with
            | Array _ ->
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
          let env =
            if is_assign then add_helper_use env "_set_slice_matrix"
            else add_helper_use env "_slice_matrix"
          in
          let env, (t, e') = check_expr env e in
          let _ =
            match t with
            | Matrix _ -> ()
            | _ -> make_err ("doubled-sliced expression " ^ string_of_expr e ^
                " is not a matrix")
          in
          let env, ss1 = check_islice env s1 in
          let env, ss2 = check_islice env s2 in
          let sslice = SSlice_Expr(SDbl_Slice((t, e'), ss1, ss2)) in
          (env, (t, sslice))
    in

    (* Check calls for built-in functions *)
    let check_builtin_call env fname args expr_s =
      let check_arg (env, checked) arg =
        let env, arg' = check_expr env arg in
        (env, arg' :: checked)
      in
      let check_n_args fn_args n_args expr_s =
        if fn_args <> n_args then
          if fn_args = 1 then make_err ("expecting 1 argument in " ^ expr_s)
          else make_err ("expecting " ^ string_of_int fn_args ^ " arguments in " ^ expr_s)
      in
      let n_args = List.length args in
      let env, args' = List.fold_left check_arg (env, []) args in
      let args' = List.rev args' in
      let arg_types = List.map fst args' in
      let env, native_fname = get_native_of_builtin env fname arg_types in
      match fname with
      | "print" | "println" ->
          let _ = check_n_args 1 n_args expr_s in
          (
            match arg_types with
            | [Void] -> make_err ("void argument in " ^ expr_s)
            (* If for some reason we want to print a built-in function, this is the only
              * case where it doesn't become restricted to an explicitly typed function;
              * in this case, just print a specialized string *)
            | [BuiltInFunc] ->
                let arg_name =
                  match snd (List.hd args') with
                  | SId s -> s
                  | _ ->
                      make_err "internal error: BuiltInFunc " ^
                      "should be direct reference to built-in"
                in
                let str = "built-in function " ^ arg_name in
                let env, native_fname = get_native_of_builtin env fname [String] in
                (env, (Void, SCall((Func([String], Void), SId native_fname),
                  [(String, SString_Lit str)])))
            | _ -> (env, (Void, SCall((Func(arg_types, Void), SId native_fname), args')))
          )
      | "deep_free" ->
          let _ = check_n_args 1 n_args expr_s in
          (
            match arg_types with
            | [Array _] ->
                (env, (Void, SCall((Func(arg_types, Void), SId native_fname), args')))
            | _ -> make_err ("non-array argument in " ^ expr_s)
          )
      | "free" ->
          let _ = check_n_args 1 n_args expr_s in
          (
            match arg_types with
            | [Matrix _] | [Array _] ->
                (env, (Void, SCall((Func(arg_types, Void), SId native_fname), args')))
            | _ -> make_err ("non-container argument in " ^ expr_s)
          )
      | "length" ->
          let _ = check_n_args 1 n_args expr_s in
          (
            match arg_types with
            | [Array _] ->
                (env, (Int, SCall((Func(arg_types, Int), SId native_fname), args')))
            | _ -> make_err ("non-array argument in " ^ expr_s)
          )
      | "rows" | "cols" ->
          let _ = check_n_args 1 n_args expr_s in
          (
            match arg_types with
            | [Matrix _] ->
                (env, (Int, SCall((Func(arg_types, Int), SId native_fname), args')))
            | _ -> make_err ("non-matrix argument in " ^ expr_s)
          )
      | "to_int" ->
          let _ = check_n_args 1 n_args expr_s in
          (
            match arg_types with
            | [Float] ->
                (env, (Int, SCall((Func(arg_types, Int), SId native_fname), args')))
            | [Matrix Float] ->
                (env, (Matrix Int, SCall((Func(arg_types, Int), SId native_fname), args')))
            | _ -> make_err ("non-float argument in " ^ expr_s)
          )
      | "to_float" ->
          let _ = check_n_args 1 n_args expr_s in
          (
            match arg_types with
            | [Int] ->
                (env, (Float, SCall((Func(arg_types, Float), SId native_fname), args')))
            | [Matrix Int] ->
                (env, (Matrix Float, SCall((Func(arg_types, Matrix Float), SId native_fname), args')))
            | _ -> make_err ("non-int argument in " ^ expr_s)
          )
      | "insert" ->
          let _ = check_n_args 3 n_args expr_s in
          (
            let e' = List.nth args' 2 in
            let err = "illegal insertion in " ^ expr_s in
            match arg_types with
            | [Array t; Int; _] ->
                let env, e' = check_assign env t e' err in
                let args' = Array.of_list args' in
                let _ = Array.set args' 2 e' in
                let args' = Array.to_list args' in
                let arg_types = Array.of_list arg_types in
                let _ = Array.set arg_types 2 (fst e') in
                let arg_types = Array.to_list arg_types in
                let env, native_fname = get_native_of_builtin env fname arg_types in
                (env, (Array t, SCall((Func(arg_types, Array t), SId native_fname), args')))
            | [Matrix t; Int; Matrix _] ->
                let env, _ = check_assign env (Matrix t) e' err in
                (env, (Matrix t, SCall((Func(arg_types, Matrix t), SId native_fname), args')))
            | _ -> make_err ("non-container argument in " ^ expr_s)
          )
      | "delete" ->
          let _ = check_n_args 2 n_args expr_s in
          (
            match arg_types with
            | [Array t; Int] ->
                (env, (Array t, SCall((Func(arg_types, Array t), SId native_fname), args')))
            | [Matrix t; Int] ->
                (env, (Matrix t, SCall((Func(arg_types, Matrix t), SId native_fname), args')))
            | _ -> make_err ("non-container argument in " ^ expr_s)
          )
      | "append" ->
          let _ = check_n_args 2 n_args expr_s in
          (
            let e' = List.nth args' 1 in
            let err = "illegal append in " ^ expr_s in
            match arg_types with
            | [Array t; _] ->
                let env, e' = check_assign env t e' err in
                let args' = Array.of_list args' in
                let _ = Array.set args' 1 e' in
                let args' = Array.to_list args' in
                let arg_types = Array.of_list arg_types in
                let _ = Array.set arg_types 1 (fst e') in
                let arg_types = Array.to_list arg_types in
                let env, native_fname = get_native_of_builtin env fname arg_types in
                (env, (Array t, SCall((Func(arg_types, Array t), SId native_fname), args')))
            | [Matrix t; Matrix _] ->
                let env, _ = check_assign env (Matrix t) e' err in
                (env, (Matrix t, SCall((Func(arg_types, Matrix t), SId native_fname), args')))
            | _ -> make_err ("non-container argument in " ^ expr_s)
          )
      | "die" ->
          let _ = check_n_args 1 n_args expr_s in
          if arg_types = [String] then
            (env, (Void, SCall((Func(arg_types, Void), SId native_fname), args')))
          else make_err ("non-string argument in " ^ expr_s)
      | _ -> make_err ("internal error: " ^ fname ^ " is not a built-in function")
    in

    let expr_s = string_of_expr expr in
    match expr with
    | Int_Lit l -> (env, (Int, SInt_Lit l))
    | Float_Lit l -> (env, (Float, SFloat_Lit l))
    | Bool_Lit l -> (env, (Bool, SBool_Lit l))
    | Noexpr -> (env, (Void, SNoexpr))
    | Id s ->
        let t = lookup env.scope s in
        let env =
          match t with
          | Array _ | Matrix _ -> add_helper_use env "_check"
          | Func(_, _) ->
              let env = { env with func_uses = StringSet.add s env.func_uses } in
              add_helper_use env "_check"
          | _ -> env
        in
        (env, (t, SId s))
    | String_Lit s -> (env, (String, SString_Lit s))
    | Array_Lit _ as a ->
        let env = add_helper_use env "_malloc_array" in
        let env = add_helper_use env "_set_array" in
        check_container_lit env a
    | Empty_Array(t, n) ->
        let env = add_helper_use env "_malloc_array" in
        let env = add_helper_use env "_init_array" in
        let env, (nt, n') = check_expr env n in
        if nt <> Int then make_err ("non-integer length specified in " ^ expr_s)
        else (env, (Array t, SEmpty_Array(t, (nt, n'))))
    | Matrix_Lit _ as m ->
        let env = add_helper_use env "_malloc_matrix" in
        let env = add_helper_use env "_set_matrix" in
        check_container_lit env m
    | Empty_Matrix(t, r, c) ->
        let env = add_helper_use env "_malloc_matrix" in
        let env = add_helper_use env "_init_matrix" in
        let env, (rt, r') = check_expr env r in
        let env, (ct, c') = check_expr env c in
        if rt <> Int || ct <> Int
        then make_err ("non-integer dimensions specified in " ^ expr_s)
        else (env, (Matrix t, SEmpty_Matrix(t, (rt, r'), (ct, c'))))
    | Index_Expr i -> check_index_expr env i false
    | Slice_Expr s -> check_slice_expr env s false
    | Assign(e1, e2) ->
        (
          match e1 with
          | Id s ->
              let lt = lookup env.scope s in
              let env, (rt, e') = check_expr env e2 in
              let err =
                "illegal assignment " ^ string_of_typ lt ^ " = " ^
                string_of_typ rt ^ " in " ^ expr_s
              in
              let env, (rt, e') = check_assign env lt (rt, e') err  in
              (env, (lt, SAssign((lt, SId s), (rt, e'))))
          | Index_Expr i ->
              let env, (lt, i') = check_index_expr env i true in
              let env, (rt, e') = check_expr env e2 in
              let err =
                "illegal assignment " ^ string_of_typ lt ^ " = " ^
                string_of_typ rt ^ " in " ^ expr_s
              in
              let env, (rt, e') = check_assign env lt (rt, e') err in
              (env, (lt, SAssign((lt, i'), (rt, e'))))
          | Slice_Expr s ->
              let env, (lt, s') = check_slice_expr env s true in
              let env, (rt, e') = check_expr env e2 in
              let err =
                "illegal assignment " ^ string_of_typ lt ^ " = " ^
                string_of_typ rt ^ " in " ^ expr_s
              in
              let env, (rt, e') = check_assign env lt (rt, e') err in
              (env, (lt, SAssign((lt, s'), (rt, e'))))
          | _ -> make_err (expr_s ^ " is not assignable")
        )
    | Unop(op, e) ->
        let env, (t, e') = check_expr env e in
        (
          match op with
          | Neg when t = Int || t = Float -> (env, (t, SUnop(op, (t, e'))))
          | Neg when t = Matrix Int || t = Matrix Float ->
              let neg_one =
                if typ_of_container t = Int then Int_Lit (-1)
                else Float_Lit "-1."
              in
              let e = Binop(neg_one, Mult, e) in
              check_expr env e
          | Not when t = Bool -> (env, (t, SUnop(op, (t, e'))))
          | _ ->
              make_err ("illegal unary operator " ^
              string_of_uop op ^ string_of_typ t ^
              " in " ^ expr_s)
        )
    | Binop(e1, op, e2) ->
        let env, (t1, e1') = check_expr env e1 in
        let env, (t2, e2') =
          match e2 with
          | One ->
              let rec get_one typ =
                match typ with
                | Int -> Int_Lit 1
                | Float -> Float_Lit "1."
                | Matrix t -> get_one t
                | _ -> make_err ("illegal increment/decrement " ^
                    string_of_typ typ ^ " in " ^ expr_s)
              in
              let one = get_one t1 in
              check_expr env one
          | _ -> check_expr env e2
        in
        let err = "illegal binary operator " ^
          string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
          string_of_typ t2 ^ " in " ^ expr_s
        in
        let check_mat_op env is_arith (t1, e1') (t2, e2') =
          (* Check is expression is unreachable after operation; we'll want to free these *)
          let is_unreachable_mat e =
            match e with
            | SMatrix_Lit _ | SIndex_Expr _ | SSlice_Expr _ | SBinop(_, _, _) -> true
            | _ -> false
          in
          let base_type t =
            match t with
            | Matrix t ->
                if t = Int || t = Float then t else make_err err
            | _ ->
                if is_arith then
                  if t = Int || t = Float then t
                  else make_err err
                else t
          in
          (* Since we're implementing broadcasting, we just need to
           * check that the base types are the same *)
          let base_t1 = base_type t1 in
          let base_t2 = base_type t2 in
          let _ = if base_t1 <> base_t2 then make_err err in
          let env, (t1, e1'), (t2, e2') =
            match t1, t2 with
            | Matrix _, Matrix _ -> env, (t1, e1'), (t2, e2')
            | Matrix _, t ->
                let env, _ = get_native_of_builtin env "free" [Matrix t] in
                env, (t1, e1'), (Matrix t2, SMatrix_Lit [| [| (t2, e2') |] |])
            | t, Matrix _ ->
                let env, _ = get_native_of_builtin env "free" [Matrix t] in
                env, (Matrix t1, SMatrix_Lit [| [| (t1, e1') |] |]), (t2, e2')
            | _, _ -> env, (t1, e1'), (t2, e2')
          in
          let env =
            if is_matrix t1 then
              let env = add_helper_use env "_mat_binop" in
              if is_unreachable_mat e1' || is_unreachable_mat e2' then
                fst (get_native_of_builtin env "free" [t1])
              else env
            else env
          in
          (env, (t1, e1'), (t2, e2'))
        in
        (* Determine expression type based on operator and operand types *)
        let env, ty, (t1, e1'), (t2, e2') =
          match op with
          | Add | Sub | Mult | Div | Mod | Exp ->
              let env =
                match op with
                | Div | Mod | Exp when t1 = t2 -> add_helper_use env "_check"
                | _ -> env
              in
              let env =
                if op = Exp && t1 = t2 then
                  if t1 = Float then
                    let env = add_helper_use env "llvm.floor.f64" in
                    add_helper_use env "_fexp"
                  else if t1 = Int then add_helper_use env "_iexp"
                  else env
                else env
              in
              let env, (t1, e1'), (t2, e2') =
                check_mat_op env true (t1, e1') (t2, e2')
              in
              (env, t1, (t1, e1'), (t2, e2'))
          | Equal | Neq ->
              (* Instead of returning matrix of bools, we return a
               * matrix of 0s and 1s *)
              let env, (t1, e1'), (t2, e2') =
                check_mat_op env false (t1, e1') (t2, e2')
              in
              if is_matrix t1 then (env, t1, (t1, e1'), (t2, e2'))
              else (env, Bool, (t1, e1'), (t2, e2'))
          | Less | Leq | Greater | Geq ->
              (* Instead of returning matrix of bools, we return a
               * matrix of 0s and 1s *)
              let env, (t1, e1'), (t2, e2') =
                check_mat_op env true (t1, e1') (t2, e2')
              in
              if is_matrix t1 then (env, t1, (t1, e1'), (t2, e2'))
              else (env, Bool, (t1, e1'), (t2, e2'))
          | And | Or when t1 = t2 && t1 = Bool -> (env, t1, (t1, e1'), (t2, e2'))
          | MatMult when t1 = t2 ->
              let env = add_helper_use env "_matmult" in
              let env, (t1, e1'), (t2, e2') =
                check_mat_op env true (t1, e1') (t2, e2')
              in
              (env, t1, (t1, e1'), (t2, e2'))
          | _ -> make_err err
        in
        (env, (ty, SBinop((t1, e1'), op, (t2, e2'))))
      | Call(f, args) ->
          let env, (t, f') = check_expr env f in

          (* Next, check if function here is a direct built-in call; otherwise,
            * if passed via function parameter/assignment to a variable, they will
            * be restricted to explicitly typed functions, so no need to perform
            * special checks *)
          let is_builtin = t = BuiltInFunc in
          if is_builtin then
            (* In this case, it's a direct call, meaning f' is a SId *)
            let fname =
              match f' with
              | SId s -> s
              | _ ->
                  make_err ("internal error: only direct calls to " ^
                  "built-in functions should have type BuiltInFunc")
            in
            check_builtin_call env fname args expr_s
          else
            let formals, return_type =
              match t with
              | Func(formals, return_type) -> (formals, return_type)
              | _ -> make_err (string_of_expr f ^ " is not a function in " ^ expr_s)
            in
            let param_length = List.length formals in
            if List.length args <> param_length then
              make_err ("expecting " ^ string_of_int param_length ^
                " arguments in " ^ expr_s)
            else
              let check_arg (env, checked) arg_type expr =
                let env, (expr_type, expr') = check_expr env expr in
                let err =
                  "illegal argument " ^ string_of_expr expr ^
                  ", found " ^ string_of_typ expr_type ^
                  ", expected " ^ string_of_typ arg_type ^
                  " in " ^ expr_s
                in
                let env, checked_arg = check_assign env arg_type (expr_type, expr') err in
                (env, checked_arg :: checked)
              in
              let env, args' = List.fold_left2 check_arg (env, []) formals args in
              let args' = List.rev args' in
              (env, (return_type, SCall((t, f'), args')))
      | One -> make_err "internal error: One should not be passed to check_expr"
      | Slice_Inc -> make_err "internal error: Slice_Inc should not be passed to check_expr"
      | End -> (env, (Int, SEnd))
  in

  (* Check that a type is validly formed *)
  let rec check_type = function
    | Matrix t when t <> Int && t <> Float -> false
    | Array t -> t <> Void && check_type t
    | Func(args, ret) ->
        List.for_all check_type args && check_type ret && List.for_all (fun x -> x <> Void) args
    | _ -> true
  in

  let add_decl scope name typ =
    let built_in_err = "identifier " ^ name ^ " may not be defined" in
    let dup_err = "duplicate identifier " ^ name in

    (* Cannot redefine built-ins *)
    let is_built_in = StringMap.mem name built_in_funcs in
    let _ = if is_built_in then make_err built_in_err in

    (* Cannot declare duplicate in same scope *)
    let is_dup = StringMap.mem name scope.variables in
    let _ = if is_dup then make_err dup_err in
    { scope with variables = StringMap.add name typ scope.variables }
  in

  (* Return semantically checked declaration *)
  let check_v_decl (env, checked) decl =
    let check_v_decl_type decl =
      let _, t, _, _ = decl in
      let err =
        "illegal declaration type " ^ string_of_typ t ^
        " in " ^ string_of_vdecl decl
      in
      if t = Void || not (check_type t) then make_err err
    in
    let add_v_decl scope decl =
      let _, t, s, _ = decl in
      add_decl scope s t
    in
    let kw, t, s, expr = decl in

    (* Check decl type is valid *)
    let _ = check_v_decl_type decl in

    (* Check keyword matches type *)
    let expr_kw =
      match t with
      | Int | Bool | Float | String | Func(_, _) -> Var
      | Exc -> Exception
      | Array _ | Matrix _ -> Create
      | Notyp -> Auto
      | _ -> make_err "internal error: check_v_decl_type should have rejected"
    in
    let kw_err =
      "illegal use of declaration keyword " ^ string_of_decl_kw kw ^
      " for type " ^ string_of_typ t ^ " in " ^ string_of_vdecl decl
    in
    let _ = if kw <> Nokw && kw <> expr_kw then make_err kw_err in

    (* Check initialization type is valid; note that an empty initialization
     * is valid, as this means we're declaring but not initializing *)
    let env, (et, expr') = check_expr env expr in
    let typ_err =
      "illegal initialization " ^ string_of_typ t ^
      " = " ^ string_of_typ et ^ " in " ^ string_of_vdecl decl
    in
    let env, (t, expr') =
      if expr' = SNoexpr then (env, (t, expr'))
      (* Auto variables must be initialized to explicitly typed expressions;
       * built-in functions require the presence of other explicitly typed
       * expressions to be explicitly typed, so clearly we cannot have both here *)
      else if t = Notyp && et = BuiltInFunc then
        make_err ("illegal initialization of auto variable in " ^
        string_of_vdecl decl)
      (* Otherwise, set the auto variable to the type of its initializer *)
      else if t = Notyp then (env, (et, expr'))
      else check_assign env t (et, expr') typ_err
    in

    (* Add decl to scope; we redefine decl here in case the variable was
     * an auto variable, meaning that the type is now changed *)
    let decl = (kw, t, s, expr) in
    let scope = add_v_decl env.scope decl in

    (* After type check, we explicitly add decl's type to the sexpr,
     * to handle the case where we have a void initialization (i.e.
     * declaration but not initialization) *)
    ({ env with scope }, (kw, t, s, (t, expr')) :: checked)
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
    let check_formal_type fname formal =
      let _, t, _, _ = formal in
      let err =
        "illegal argument type " ^ string_of_typ t ^ " in " ^
        string_of_vdecl formal ^ " for the function " ^ fname
      in
      if t = Void || not (check_type t) then make_err err
    in
    (* Return a semantically-checked statement, along with a bool
     * indicating if there was at least one return statement
     * (somewhere in the statement itself *)
    let rec check_stmt env stmt =
      let check_bool_expr env e =
        let env, (t', e') = check_expr env e
        and err = "expected Boolean expression in " ^ string_of_expr e
        in
        if t' <> Bool then make_err err else (env, (t', e'))
      in
      match stmt with
      | Expr e ->
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
          let err =
            "return gives " ^ string_of_typ t ^ " expected " ^
            string_of_typ func.typ ^ " in " ^ string_of_expr e
          in
          let env, (t, e') = check_assign env func.typ (t, e') err in
          (env, SReturn(t, e'), true)
      (* A block is correct if each statement is correct and nothing
      * follows any return statement. Blocks define their own scope. *)
      | Block sl ->
          let parent_scope = env.scope in
          let scope = { variables = StringMap.empty; parent = Some parent_scope } in
          let rec check_stmt_list env = function
            | [Return _ as s] ->
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
          let env, checked = check_v_decl (env, []) d in
          let sdecl = List.hd checked in
          (env, SDecl sdecl, false)
      | _ -> make_err "not supported yet in check_stmt"
    in

    (* Rename main function to prog_main *)
    let old_fname = func.fname in
    let fname = if old_fname = sys_main then prog_main else old_fname in
    let func = { func with fname } in

    (* Check formals have valid type *)
    let _ = List.iter (check_formal_type old_fname) func.formals in

    (* Check return type is valid *)
    let err =
      "illegal return type " ^ string_of_typ func.typ ^
      " for the function " ^ old_fname
    in
    let _ = if not (check_type func.typ) then make_err err in

    (* Add function decl to parent scope *)
    let parent_scope = env.scope in
    let parent_scope = add_func_decl parent_scope func in

    (* Build local symbol table of variables for this function *)
    let scope = { variables = StringMap.empty; parent = Some parent_scope } in
    let env = { env with scope } in
    let env, formals' = List.fold_left check_v_decl (env, []) func.formals in
    let formals' = List.rev formals' in

    (* Check body *)
    let env, body =
      match check_stmt env (Block func.body) with
      | env, SBlock sl, true -> (env, sl)
      | env, SBlock sl, false ->
          let err =
            "function " ^ old_fname ^ " has return type " ^
            string_of_typ func.typ ^ " but no return statement found"
          in
          if func.typ <> Void then make_err err else (env, sl)
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
  let env =
    {
      scope = global_scope;
      func_uses = StringSet.empty;
      builtin_uses = StringSet.empty;
      helper_uses = StringSet.empty;
    }
  in
  let env, globals' = List.fold_left check_v_decl (env, []) globals in
  let env, functions' = List.fold_left check_func_decl (env, []) functions in

  (* Ensure "main" is defined *)
  let _ =
    let main_err = "must have a main function of type func<():void>" in
    try
      let typ = lookup env.scope prog_main in
      match typ with
      | Func([], Void) -> ()
      | _ -> make_err main_err
    with Not_found -> make_err main_err
  in
  (* Mark program main as used, since system main will call it *)
  let env = { env with func_uses = StringSet.add prog_main env.func_uses } in

  let program = (List.rev globals', List.rev functions') in
  (env, program)
