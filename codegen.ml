(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

(* We'll refer to Llvm and Ast constructs with module names *)
(* TODO: make a utility module of some sort *)
module S = Semant
module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)
module TypeMap = Map.Make(struct type t = A.typ let compare = compare end)

type symbol_table = {
  variables : L.llvalue StringMap.t;
  parent : symbol_table option
}

let make_err err = raise (Failure err)

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let translate (array_types, program) =
  let globals, functions = program in
  let context    = L.global_context () in
  (* Create an LLVM module -- this is a "container" into which we'll
    generate actual code *)
  let the_module = L.create_module context "Neo" in

  (* Add types to the context so we can use them in our LLVM code *)
  let i64_t      = L.i64_type    context
  and i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type   context
  and pointer_t  = L.pointer_type in

  let matrixi_t =
    let matrix_type = L.named_struct_type context "matrixi_t" in
    let body_type = pointer_t (pointer_t i32_t) in
    (* Struct: double pointer to first element, nrows, ncols *)
    let _ = L.struct_set_body matrix_type [| body_type ; i32_t ; i32_t |] false in
    matrix_type
  in

  let mat_float_t =
    let matrix_type = L.named_struct_type context "mat_float_t" in
    let body_type = pointer_t (pointer_t float_t) in
    (* Struct: double pointer to first element, nrows, ncols *)
    let _ = L.struct_set_body matrix_type [| body_type ; i32_t ; i32_t |] false in
    matrix_type
  in

  let array_t =
    let array_type = L.named_struct_type context "array_t" in
    let body_type = pointer_t (pointer_t i8_t) in
    (* Struct: void pointer to first element, element size, length *)
    let _ = L.struct_set_body array_type [| body_type ; i64_t ; i32_t |] false in
    array_type
  in

  let matrix_t typ =
    match typ with
        A.Int -> matrixi_t
      | A.Float -> mat_float_t
      | _ -> make_err "internal error: semant should have rejected invalid matrix type"
  in

  let ltype_of_typ = function
      A.Int      -> i32_t
    | A.Bool     -> i1_t
    | A.Float    -> float_t
    | A.Void     -> void_t
    | A.String   -> pointer_t i8_t
    | A.Array _  -> pointer_t array_t
    | A.Matrix t -> pointer_t (matrix_t t)
    | _ -> make_err "not supported yet in ltype_of_typ"
  in

  (* We wrap the program's main function call inside of another
   * true system main function; we rename the program's main function
   * as "prog_main" when generating the LLVM code. The system main
   * will also be responsible for initializing any global variables.
   * See semant for this in action. *)
  let sys_main = "main" in
  let prog_main = "prog_main" in

  (* Declare system main + its builder *)
  let main_t = L.function_type i32_t [||] in
  let main = L.define_function sys_main main_t the_module in
  let main_builder = L.builder_at_end context (L.entry_block main) in

  (* Built-in format strings *)
  let empty_str = L.build_global_stringptr "" "empty_string" main_builder in
  let int_format_str = L.build_global_stringptr "%d" "fmt" main_builder in
  let float_format_str = L.build_global_stringptr "%g" "fmt" main_builder in

  (* Declare built-in functions *)
  let printf_t = L.var_arg_function_type i32_t [| pointer_t i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let print_element_t = L.function_type void_t [| pointer_t i8_t |] in
  let print_array_t =
    L.function_type void_t [| pointer_t array_t ; pointer_t print_element_t |]
  in
  let print_array_func = L.declare_function "print_array" print_array_t the_module in

  let print_matrixi_t = L.function_type void_t [| pointer_t matrixi_t ; i1_t |] in
  let print_matrixi_func = L.declare_function "print_matrixi" print_matrixi_t the_module in

  let init_matrixi_t = L.function_type void_t [| pointer_t matrixi_t |] in
  let init_matrixi_func = L.declare_function "init_matrixi" init_matrixi_t the_module in

  let set_ptrs_matrixi_t = L.function_type void_t [| pointer_t matrixi_t ; pointer_t i32_t |] in
  let set_ptrs_matrixi_func = L.declare_function "set_ptrs_matrixi" set_ptrs_matrixi_t the_module in

  let set_ptrs_array_t = L.function_type void_t [| pointer_t array_t ; pointer_t i8_t |] in
  let set_ptrs_array_func = L.declare_function "set_ptrs_array" set_ptrs_array_t the_module in

  (* Build any necessary element-printing functions for array types *)
  let rec make_element_print_func array_type element_print_funcs =
    let typ = A.typ_of_arr_typ array_type in
    if TypeMap.mem array_type element_print_funcs then element_print_funcs
    else
      let print_t = L.function_type void_t [| pointer_t i8_t |] in
      let print_func = L.define_function ("print_" ^ A.string_of_typ typ) print_t the_module in
      let builder = L.builder_at_end context (L.entry_block print_func) in
      let ltype = ltype_of_typ typ in

      (* Cast and load param *)
      let param = (L.params print_func).(0) in
      let param = L.build_bitcast param (pointer_t ltype) "param" builder in
      let param = L.build_load param "param" builder in

      (* Call corresponding print function; if we have another array, recursively make it *)
      let element_print_funcs = match typ with
          A.Array _ -> make_element_print_func typ element_print_funcs
        | _ -> element_print_funcs
      in
      let _ = match typ with
          A.Int | A.Bool ->
            L.build_call printf_func
            [| int_format_str ; param |] "printf" builder
        | A.Float ->
            L.build_call printf_func
            [| float_format_str ; param |] "printf" builder
        | A.String -> L.build_call printf_func [| param |] "printf" builder
        | A.Array _ ->
            L.build_call print_array_func
            [| param ; TypeMap.find typ element_print_funcs |] "" builder
        | A.Matrix t ->
            (
              (* Print flat matrices if embedded within arrays *)
              match t with
                  A.Int ->
                    L.build_call print_matrixi_func
                    [| param ; L.const_int i1_t 1 |] "" builder
                | _ -> make_err "not supported yet in print (matrix)"
            )
        | _ -> make_err "not supported yet in print"
      in
      let _ = L.build_ret_void builder in
      TypeMap.add array_type print_func element_print_funcs
  in
  let element_print_funcs = S.TypeSet.fold make_element_print_func array_types TypeMap.empty in

  (* Returns initial value for an empty declaration of a given type *)
  let init t = match t with
      A.Float -> L.const_float float_t 0.0
    | A.Int -> L.const_int i32_t 0
    | A.String -> empty_str
    | A.Array _ -> L.const_null (pointer_t array_t)
    | A.Matrix t -> L.const_null (pointer_t (matrix_t t))
    | _ -> make_err "not supported yet in init"
  in

  (* Returns value of an identifier *)
  let rec lookup name scope =
    (* Redirect any attempt to call the program main through it's raw "main" name
     * to the renamed program main *)
    let name = if name = sys_main then prog_main else name in
    try StringMap.find name scope.variables
    with Not_found ->
      match scope.parent with
          None -> make_err "internal error: semant should have rejected on undeclared identifier"
        | Some parent -> lookup name parent
  in

  let add_func_decl scope fdecl =
    let fname = if fdecl.sfname = sys_main then prog_main else fdecl.sfname in
    let return_type = fdecl.styp in
    let get_decl_type (_, typ, _, _) = typ in
    let arg_types = List.map get_decl_type fdecl.sformals in
    let arg_types = Array.of_list (List.map ltype_of_typ arg_types) in
    let typ = L.function_type (ltype_of_typ return_type) arg_types in
    let f = L.define_function fname typ the_module in
    { scope with variables = StringMap.add fname f scope.variables }
  in

  (* Construct code for an expression; return its value *)
  let rec expr scope builder (t, e) =
    (* TODO: make sure to free any malloc'd arrays/matrices and move to stack var *)
    let make_array typ length builder =
      let ltype = ltype_of_typ typ in
      let size = L.size_of ltype in

      (* Allocate required space *)
      let body_ptr = L.build_array_alloca (pointer_t i8_t) length "body_ptr" builder in
      let arr_ptr = L.build_alloca array_t "arr_ptr" builder in

      (* Set fields *)
      let arr_body = L.build_struct_gep arr_ptr 0 "arr_body" builder in
      let arr_size = L.build_struct_gep arr_ptr 1 "arr_size" builder in
      let arr_length = L.build_struct_gep arr_ptr 2 "arr_length" builder in
      let _ = L.build_store body_ptr arr_body builder in
      let _ = L.build_store size arr_size builder in
      let _ = L.build_store length arr_length builder in
      arr_ptr
    in

    let make_array_lit typ raw_elements builder =
      let ltype = ltype_of_typ typ in
      let length = L.const_int i32_t (Array.length raw_elements) in
      let arr_ptr = make_array typ length builder in

      (* Allocate and fill array body *)
      let body = L.build_array_alloca ltype length "body" builder in
      let set_element i element =
        let ptr =
          L.build_in_bounds_gep body
          [| L.const_int i32_t i |] "ptr" builder
        in
        let _ = L.build_store element ptr builder in
        ()
      in
      let _ = Array.iteri set_element raw_elements in

      (* Cast body ptr to "void pointer" *)
      let body_ptr = L.build_bitcast body (pointer_t i8_t) "body_ptr" builder in

      (* Set body pointers *)
      let _ = L.build_call set_ptrs_array_func [| arr_ptr ; body_ptr |] "" builder in
      arr_ptr
    in

    let make_empty_array typ length builder =
      let ltype = ltype_of_typ typ in
      let arr_ptr = make_array typ length builder in

      (* Allocate array contents *)
      let body = L.build_array_alloca ltype length "body" builder in

      (* Cast body ptr to "void pointer" *)
      let body_ptr = L.build_bitcast body (pointer_t i8_t) "body_ptr" builder in

      (* Set body pointers *)
      let _ = L.build_call set_ptrs_array_func [| arr_ptr ; body_ptr |] "" builder in
      arr_ptr
    in

    let make_matrix typ rows cols builder =
      let ltype = ltype_of_typ typ in

      (* Allocate required space *)
      let row_ptrs = L.build_array_alloca (pointer_t ltype) rows "row_ptrs" builder in
      let mat_ptr = L.build_alloca (matrix_t typ) "mat_ptr" builder in

      (* Set fields *)
      let mat_body = L.build_struct_gep mat_ptr 0 "mat_body" builder in
      let mat_rows = L.build_struct_gep mat_ptr 1 "mat_rows" builder in
      let mat_cols = L.build_struct_gep mat_ptr 2 "mat_cols" builder in
      let _ = L.build_store row_ptrs mat_body builder in
      let _ = L.build_store rows mat_rows builder in
      let _ = L.build_store cols mat_cols builder in
      mat_ptr
    in

    let make_matrix_lit typ raw_elements builder =
      let ltype = ltype_of_typ typ in
      let rows = Array.length raw_elements in
      let cols = Array.length raw_elements.(0) in
      let mat_ptr = make_matrix typ (L.const_int i32_t rows) (L.const_int i32_t cols) builder in

      (* Allocate and fill matrix body *)
      let raw_rows = Array.map (L.const_array ltype) raw_elements in
      let raw_matrix = L.const_array (L.array_type ltype cols) raw_rows in
      let body = L.build_alloca (L.array_type (L.array_type ltype cols) rows) "body" builder in
      let _ = L.build_store raw_matrix body builder in
      let body_ptr =
        L.build_in_bounds_gep body
        [| L.const_int i32_t 0 ; L.const_int i32_t 0 ; L.const_int i32_t 0 |] "body_ptr" builder
      in

      (* Set body pointers *)
      let _ = L.build_call set_ptrs_matrixi_func [| mat_ptr ; body_ptr |] "" builder in
      mat_ptr
    in

    let make_empty_matrix typ rows cols builder =
      let ltype = ltype_of_typ typ in
      let size = L.build_mul rows cols "size" builder in
      let mat_ptr = make_matrix typ rows cols builder in

      (* Allocate matrix body *)
      let body_ptr = L.build_array_alloca ltype size "body_ptr" builder in

      (* Set body pointers and fill matrix contents *)
      let _ = L.build_call set_ptrs_matrixi_func [| mat_ptr ; body_ptr |] "" builder in
      let _ = L.build_call init_matrixi_func [| mat_ptr |] "" builder in
      mat_ptr
    in

    match e with
        SInt_Lit i -> L.const_int i32_t i
      | SBool_Lit b -> L.const_int i1_t (if b then 1 else 0)
      | SFloat_Lit l -> L.const_float_of_string float_t l
      | SString_Lit s -> L.build_global_stringptr (Scanf.unescaped s) "str" builder
      | SArray_Lit l ->
          let raw_array = Array.map (expr scope builder) l in
          let typ = A.typ_of_arr_typ t in
          make_array_lit typ raw_array builder
      | SEmpty_Array(t, n) ->
          let length = expr scope builder n in
          make_empty_array t length builder
      | SMatrix_Lit l ->
          let raw_elements = Array.map (Array.map (expr scope builder)) l in
          let typ = A.typ_of_mat_typ t in
          make_matrix_lit typ raw_elements builder
      | SEmpty_Matrix(t, r, c) ->
          let rows = expr scope builder r in
          let cols = expr scope builder c in
          make_empty_matrix t rows cols builder
      | SNoexpr -> init t
      | SId s -> L.build_load (lookup s scope) s builder
      | SAssign(e1, e2) ->
          let s =
            let _, e1 = e1 in
            match e1 with
                SId s -> s
              | _ -> make_err "not yet supported in expr"
          in
          let e' = expr scope builder e2 in
          let _  = L.build_store e' (lookup s scope) builder in e'
      | SBinop(e1, op, e2) ->
          let t, _ = e1
          and e1' = expr scope builder e1
          and e2' = expr scope builder e2
          in
          if t = A.Float then (match op with
            A.Add     -> L.build_fadd
          | A.Sub     -> L.build_fsub
          | A.Mult    -> L.build_fmul
          | A.Div     -> L.build_fdiv
          | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq     -> L.build_fcmp L.Fcmp.One
          | A.Less    -> L.build_fcmp L.Fcmp.Olt
          | A.Leq     -> L.build_fcmp L.Fcmp.Ole
          | A.Greater -> L.build_fcmp L.Fcmp.Ogt
          | A.Geq     -> L.build_fcmp L.Fcmp.Oge
          | A.And | A.Or ->
              make_err "internal error: semant should have rejected and/or on float"
          | _ -> make_err "binop not supported yet"
          ) e1' e2' "tmp" builder
          else (match op with
          | A.Add     -> L.build_add
          | A.Sub     -> L.build_sub
          | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
          | A.Equal   -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
          | A.Leq     -> L.build_icmp L.Icmp.Sle
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.Geq     -> L.build_icmp L.Icmp.Sge
          | _ -> make_err "binop not supported yet"
          ) e1' e2' "tmp" builder
      | SUnop(op, e) ->
          let (t, _) = e in
          let e' = expr scope builder e in
          (match op with
              A.Neg when t = A.Float -> L.build_fneg
            | A.Neg                  -> L.build_neg
            | A.Not                  -> L.build_not) e' "tmp" builder
      | SCall("print", [e]) ->
          let e' = expr scope builder e in
          (
            match t with
                A.Int | A.Bool ->
                  L.build_call printf_func
                  [| int_format_str ; e' |] "printf" builder
              | A.Float ->
                  L.build_call printf_func
                  [| float_format_str ; e' |] "printf" builder
              | A.String ->L.build_call printf_func [| e' |] "printf" builder
              | A.Array _ ->
                  L.build_call print_array_func
                  [| e' ; TypeMap.find t element_print_funcs |] "" builder
              | A.Matrix t ->
                  (
                    match t with
                        A.Int ->
                          L.build_call print_matrixi_func
                          [| e' ; L.const_int i1_t 0 |] "" builder
                      | _ -> make_err "not supported yet in print (matrix)"
                  )
              | _ -> make_err "not supported yet in print"
          )
      | SCall(fname, args) ->
          let f = lookup fname scope in
          let args = List.rev (List.map (expr scope builder) (List.rev args)) in
          let result = (match t with
              A.Void -> ""
            | _ -> fname ^ "_result")
          in
          L.build_call f (Array.of_list args) result builder
  in

  (* Fill in the body of the given function *)
  let build_function_body scope fdecl =
    let the_function = lookup fdecl.sfname scope in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let scope =
      let scope = { variables = StringMap.empty ; parent = Some scope } in
      let add_formal scope formal param =
        let _, t, s, _ = formal in
        let () = L.set_value_name s param in
	      let local = L.build_alloca (ltype_of_typ t) s builder in
        let _  = L.build_store param local builder in
        { scope with variables = StringMap.add s local scope.variables }
      in
      let params = Array.to_list (L.params the_function) in
      List.fold_left2 add_formal scope fdecl.sformals params
    in

    (* Each basic block in a program ends with a "terminator" instruction i.e.
    one that ends the basic block. By definition, these instructions must
    indicate which basic block comes next -- they typically yield "void" value
    and produce control flow, not values *)
    (* Invoke "instr builder" if the current block doesn't already
      have a terminator (e.g., a branch). *)
    let add_terminal builder instr =
      (* The current block where we're inserting instr *)
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder)
    in

    (* Build the code for the given statement; return the builder for
      the statement's successor (i.e., the next instruction will be built
      after the one generated by this call) *)
    (* Imperative nature of statement processing entails imperative OCaml *)
    let rec stmt (scope, builder) = function
        SBlock sl ->
          let parent_scope = scope in
          let scope = { variables = StringMap.empty ; parent = Some parent_scope } in
          let _, builder = List.fold_left stmt (scope, builder) sl in
          (* Return to parent scope *)
          (parent_scope, builder)
        (* Generate code for this expression, return resulting builder *)
      | SExpr e -> let _ = expr scope builder e in (scope, builder)
      | SReturn e ->
          let _ =
            match fdecl.styp with
                (* Special "return nothing" instr *)
                A.Void -> L.build_ret_void builder
                (* Build return statement *)
              | _ -> L.build_ret (expr scope builder e) builder
          in
          (scope, builder)
      (* The order that we create and add the basic blocks for an If statement
      doesnt 'really' matter (seemingly). What hooks them up in the right order
      are the build_br functions used at the end of the then and else blocks (if
      they don't already have a terminator) and the build_cond_br function at
      the end, which adds jump instructions to the "then" and "else" basic blocks *)
      | SIf(predicate, then_stmt, else_stmt) ->
          let bool_val = expr scope builder predicate in
          (* Add "merge" basic block to our function's list of blocks *)
          let merge_bb = L.append_block context "merge" the_function in
          (* Partial function used to generate branch to merge block *)
          let branch_instr = L.build_br merge_bb in
          (* Same for "then" basic block *)
          let then_bb = L.append_block context "then" the_function in
          (* Position builder in "then" block and build the statement *)
          let scope, then_builder =
            stmt (scope, (L.builder_at_end context then_bb)) then_stmt
          in
          (* Add a branch to the "then" block (to the merge block)
            if a terminator doesn't already exist for the "then" block *)
          let () = add_terminal then_builder branch_instr in
          (* Identical to stuff we did for "then" *)
          let else_bb = L.append_block context "else" the_function in
          let scope, else_builder =
            stmt (scope, (L.builder_at_end context else_bb)) else_stmt
          in
          let () = add_terminal else_builder branch_instr in
          (* Generate initial branch instruction perform the selection of "then"
          or "else". Note we're using the builder we had access to at the start
          of this alternative. *)
          let _ = L.build_cond_br bool_val then_bb else_bb builder in
          (* Move to the merge block for further instruction building *)
          (scope, L.builder_at_end context merge_bb)

      | SWhile(predicate, body) ->
          (* First create basic block for condition instructions -- this will
          serve as destination in the case of a loop *)
          let pred_bb = L.append_block context "while" the_function in
          (* In current block, branch to predicate to execute the condition *)
          let _ = L.build_br pred_bb builder in
          (* Create the body's block, generate the code for it, and add a branch
          back to the predicate block (we always jump back at the end of a while
          loop's body, unless we returned or something) *)
          let body_bb = L.append_block context "while_body" the_function in
          let scope, while_builder =
            stmt (scope, (L.builder_at_end context body_bb)) body
          in
          let () = add_terminal while_builder (L.build_br pred_bb) in
          (* Generate the predicate code in the predicate block *)
          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val = expr scope pred_builder predicate in
          (* Hook everything up *)
          let merge_bb = L.append_block context "merge" the_function in
          let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
          (scope, L.builder_at_end context merge_bb)
      | SDecl sd ->
          let _, t, s, e = sd in
          let e' = expr scope builder e in
          let local = L.build_alloca (ltype_of_typ t) s builder in
          let _  = L.build_store e' local builder in
          let scope =
            { scope with variables = StringMap.add s local scope.variables }
          in
          (scope, builder)
    in

    (* Build the code for each statement in the function *)
    let _, builder = stmt (scope, builder) (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  (* Build system main *)
  let build_main scope globals builder =
    let init_global sdecl =
      let _, _, s, e = sdecl in
      let e' = expr scope builder e in
      let _ = L.build_store e' (lookup s scope) builder in
      ()
    in
    let _ = List.iter init_global globals in
    let _ = expr scope builder (A.Void, SCall ("main", [])) in
    L.build_ret (L.const_int i32_t 0) builder
  in

  let global_scope =
    let global_scope = { variables = StringMap.empty ; parent = None } in
    let add_global_decl scope sdecl =
      let _, t, s, _ = sdecl in
      let init = init t in
      let global = L.define_global s init the_module in
      { scope with variables = StringMap.add s global scope.variables }
    in
    List.fold_left add_global_decl global_scope globals
  in
  let global_scope = List.fold_left add_func_decl global_scope functions in
  let _ = List.iter (build_function_body global_scope) functions in
  let _ = build_main global_scope globals main_builder in
  the_module
