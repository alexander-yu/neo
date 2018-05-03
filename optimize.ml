(* Optimization pass for the Neo compiler; forms a dependency graph
 * of the SAST and removes any functions that would not be executed
 * during the program; see the decription above prune_uses *)

open Ast
open Sast

module S = Semant

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

let make_err err = raise (Failure err)

(* Checks if the result of a matrix assign operation is unreachable if not immediately
 * referenced by a variable; these expressions create new matrices *)
let is_unreachable_mat_assign a =
  let a, ae =
    match a with
    | SAssign(a, ae) -> (snd a, snd ae)
    | _ -> make_err ("internal error: is_unreachable_mat_assign given non-SAssign")
  in
  let is_slice =
    match a with
    | SSlice_Expr _ -> true
    | _ -> false
  in
  match ae with
  | SSlice_Expr _ | SBinop(_, _, _, _) -> is_slice
  | _ -> false

let get_uses graph name =
  try StringMap.find name graph with
  | Not_found -> StringSet.empty

let is_internal s =
  s = "llvm.floor.f64" || Str.string_match (Str.regexp "_.*") s 0

let is_helper s =
  Str.string_match (Str.regexp "_h_.*") s 0

let unmangle_helper s =
  let r = Str.regexp "_h_\\(.*\\)" in
  let _ = Str.string_match r s 0 in
  let fname = Str.matched_group 1 s in
  if fname = "llvm.floor.f64" then fname
  else "_" ^ fname

let add_use user graph use =
  let use = if is_helper use then unmangle_helper use else use in
  let uses = get_uses graph user in
  let uses = StringSet.add use uses in
  let graph = StringMap.add user uses graph in
  graph

let add_uses graph user uses =
  StringSet.fold (fun use graph -> add_use user graph use) uses graph

(* Performs DFS on the dependency graph; visited nodes are marked as used *)
let rec search graph stack used =
  let visit_use use (stack, used) =
    if StringSet.mem use used then (stack, used)
    else
      (use :: stack, used)
  in
  match stack with
  | [] -> used
  | user :: stack ->
      let uses = get_uses graph user in
      let stack, used = StringSet.fold visit_use uses (stack, used) in
      let used = StringSet.add user used in
      search graph stack used

(* Forms a dependency graph of the program; in particular, it tracks
 * which functions are referenced in other functions. If a function is
 * is not referenced, it's dead code and we can eliminate it from the
 * SAST. We do this via graph search with the system main as our root node.
 * Any visited node is marked as used, and all other nodes are dead.
 * We only track function uses, as this approach to regular variables
 * may be too aggressive; for example, if we declare something like
 * auto v = f() where f is a function that also performs some sort of
 * side-effect (like printing a result) and v is "dead", then removing this
 * declaration also has the effect of removing f's side-effect, meaning that our
 * "optimization" has now changed the actual execution of the program, which is
 * not what we want. Simply tracking function dependencies is a conservative
 * enough approach to avoid this issue, as we are simply eliminating entire
 * blocks of code that would have never been executed in the first place. *)
let prune_uses program =
  let sys_main = "main" in
  let prog_main = "prog_main" in
  let globals, functions = program in

  let rec get_expr_uses (t, e) =
    let add_expr_uses uses expr =
      let expr_uses = get_expr_uses expr in
      StringSet.union uses expr_uses
    in

    let add_islice_uses uses i =
      match i with
      | SIndex i -> add_expr_uses uses i
      | SSlice(i, j) ->
          let uses = add_expr_uses uses i in
          let uses = add_expr_uses uses j in
          uses
    in

    let add_index_expr_uses uses i is_assign =
      match i with
      | SSgl_Index(e, i) ->
          let uses = add_expr_uses uses e in
          let uses = add_islice_uses uses i in
          if is_assign then StringSet.add "_h_set_array" uses
          else StringSet.add "_h_get_array" uses
      | SDbl_Index(e, i1, i2) ->
          let uses = add_expr_uses uses e in
          let uses = add_islice_uses uses i1 in
          let uses = add_islice_uses uses i2 in
          if is_assign then StringSet.add "_h_set_matrix" uses
          else StringSet.add "_h_get_matrix" uses
    in

    let add_slice_expr_uses uses s is_assign =
      match s with
      | SSgl_Slice(e, s) ->
          let uses = add_expr_uses uses e in
          let uses = add_islice_uses uses s in
          if is_assign then StringSet.add "_h_set_slice_array" uses
          else StringSet.add "_h_slice_array" uses
      | SDbl_Slice(e, s1, s2) ->
          let uses = add_expr_uses uses e in
          let uses = add_islice_uses uses s1 in
          let uses = add_islice_uses uses s2 in
          if is_assign then StringSet.add "_h_set_slice_matrix" uses
          else StringSet.add "_h_slice_matrix" uses
    in

    let helpers_of_internal fname =
      let rec typ_contains_class typ typ_class =
        match typ, typ_class with
        | Matrix _, "matrix" -> true
        | Func(_, _), "function" -> true
        | Array _, "array" -> true
        | Array t, _ -> typ_contains_class t typ_class
        | _ -> string_of_typ typ = typ_class
      in
      let fname_fragments = Str.split (Str.regexp "_") fname in
      (* We add an extra "_h" prefix to indicate that it's a helper; this is useful for
       * the optimize pass, so we can distinguish between built-in implementations and
       * internal helper functions *)
      match fname_fragments with
      | ["print"; arg_type] | ["println"; arg_type] ->
          let arg_type = typ_of_string arg_type in
          (
            match arg_type with
            | Array t when typ_contains_class t "matrix" -> ["_h_print_array"; "_h_print_flat_matrix"]
            | Array t when typ_contains_class t "function" -> ["_h_print_array"; "_h_print_function"]
            | Array _ -> ["_h_print_array"]
            | _ -> []
          )
      (* Note we don't perform validation that we're being handed an array; validation on
      * argument types is already done elsewhere *)
      | ["deep"; "free"; _] -> ["_h_deep_free_array"]
      | [("insert" as fname); arg_type] | [("append" as fname); arg_type] ->
          let arg_type = typ_of_string arg_type in
          if is_array arg_type then ["_h_" ^ fname ^ "_array"] else []
      | ["read"; "fmat"; _] | ["read"; "imat"; _] -> ["_h_read_mat"]
      | _ -> []
    in

    let uses = StringSet.empty in
    match e with
    | SInt_Lit _ | SFloat_Lit _ | SBool_Lit _ | SString_Lit _  -> uses
    | SSlice_Inc | SEnd | SNoexpr -> uses
    | SId s ->
        let uses =
          match t with
          | Func(_, _) ->
              let uses = StringSet.add "_h_check" uses in
              StringSet.add s uses
          | _ -> uses
        in
        let add_internal_uses uses internal_fname =
          let helpers = helpers_of_internal internal_fname in
          StringSet.union uses (StringSet.of_list helpers)
        in
        if is_internal s then add_internal_uses uses s
        else uses
    | SArray_Lit l ->
        let uses = StringSet.add "_h_malloc_array" uses in
        let uses = StringSet.add "_h_set_array" uses in
        Array.fold_left add_expr_uses uses l
    | SEmpty_Array(_, n) ->
        let uses = StringSet.add "_h_malloc_array" uses in
        let uses = StringSet.add "_h_init_array" uses in
        add_expr_uses uses n
    | SMatrix_Lit l ->
        let add_row_uses uses row =
          Array.fold_left add_expr_uses uses row
        in
        let uses = StringSet.add "_h_malloc_matrix" uses in
        let uses = StringSet.add "_h_set_matrix" uses in
        Array.fold_left add_row_uses uses l
    | SEmpty_Matrix(_, r, c) ->
        let uses = StringSet.add "_h_malloc_matrix" uses in
        let uses = StringSet.add "_h_init_matrix" uses in
        let uses = add_expr_uses uses r in
        let uses = add_expr_uses uses c in
        uses
    | SIndex_Expr i -> add_index_expr_uses uses i false
    | SSlice_Expr s -> add_slice_expr_uses uses s false
    | SBinop(e1, op, e2, _) ->
        let uses = add_expr_uses uses e1 in
        let uses = add_expr_uses uses e2 in
        let uses =
          if is_matrix (fst e1) then
            let uses = StringSet.add "_h_mat_binop" uses in
            let type_tag = string_of_typ (fst e1) in
            (* It's possible we won't need to free a matrix here,
             * but there's a good chance we may need to, so we include it
             * just in a case, as it's a situational use that could be
             * called in codegen *)
            let internal_fname = "_free_" ^ type_tag in
            StringSet.add internal_fname uses
          else uses
        in
        (
          match op with
          | Div | Mod when fst e1 = Int || fst e1 = Float ->
              StringSet.add "_h_check" uses
          | Exp when fst e1 = Float ->
              let uses = StringSet.add "_h_check" uses in
              let uses = StringSet.add "_h_llvm.floor.f64" uses in
              let uses = StringSet.add "_h_fexp" uses in
              uses
          | Exp when fst e1 = Int ->
              let uses = StringSet.add "_h_check" uses in
              let uses = StringSet.add "_h_iexp" uses in
              uses
          | MatMult -> StringSet.add "_h_matmult" uses
          | _ -> uses
        )
    | SUnop(_, e) -> add_expr_uses uses e
    | SAssign(e1, e2) ->
        let uses = add_expr_uses uses e2 in
        (
          match snd e1 with
          | SId _ -> add_expr_uses uses e1
          | SIndex_Expr i -> add_index_expr_uses uses i true
          | SSlice_Expr s -> add_slice_expr_uses uses s true
          | _ -> make_err "internal error: given invalid assign expression"
        )
    | SCall(f, args) ->
        let uses = add_expr_uses uses f in
        List.fold_left add_expr_uses uses args
  in

  let get_function_uses func =
    let rec get_stmt_uses stmt =
      let add_stmt_uses uses stmt =
        let stmt_uses = get_stmt_uses stmt in
        StringSet.union uses stmt_uses
      in

      match stmt with
      | SExpr e ->
          let uses = get_expr_uses e in
          (
            match e with
            | Matrix _, (SAssign(_, _) as a) ->
                if is_unreachable_mat_assign a then
                  let type_tag = string_of_typ (fst e) in
                  let internal_fname = "_free_" ^ type_tag in
                  StringSet.add internal_fname uses
                else uses
            | _ -> uses
          )
      | SIf(p, b1, b2) ->
          let uses = get_expr_uses p in
          let uses = add_stmt_uses uses b1 in
          let uses = add_stmt_uses uses b2 in
          uses
      | SWhile(p, s) ->
          let uses = get_expr_uses p in
          add_stmt_uses uses s
      | SReturn e -> get_expr_uses e
      | SBlock sl ->
          List.fold_left add_stmt_uses StringSet.empty sl
      | SDecl d ->
          let _, _, _, e = d in
          get_expr_uses e
    in

    get_stmt_uses (SBlock func.sbody)
  in

  let add_global_uses uses decl =
    let _, _, _, e = decl in
    let global_uses = get_expr_uses e in
    StringSet.union uses global_uses
  in

  let add_function_uses graph func =
    let fname = func.sfname in
    let func_uses = get_function_uses func in
    add_uses graph fname func_uses
  in

  (* The system main is responsible for calling the program main,
   * as well as initializing any global variables, so collect all uses
   * for these and register them under the system main. *)
  let main_uses = List.fold_left add_global_uses StringSet.empty globals in
  let main_uses = StringSet.add prog_main main_uses in
  let graph = add_uses StringMap.empty sys_main main_uses in
  let graph = List.fold_left add_function_uses graph functions in

  (* Search graph with system main as source node *)
  let used = StringSet.add sys_main StringSet.empty in
  let used = search graph [sys_main] used in

  (* Filter out functions that are unused *)
  let is_used func = StringSet.mem func.sfname used in
  let is_used_internal fname =
    is_internal fname && StringSet.mem fname used
  in
  let functions = List.filter is_used functions in
  let program = (globals, functions) in

  let internal_uses = StringSet.filter is_used_internal used in
  (internal_uses, program)
