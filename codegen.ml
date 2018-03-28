(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

(* We'll refer to Llvm and Ast constructs with module names *)
module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

type symbol_table = {
  variables : L.llvalue StringMap.t;
  parent : symbol_table option
}

let make_err err = raise (Failure err)

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let translate (globals, functions) =
  let context    = L.global_context () in
  (* Create an LLVM module -- this is a "container" into which we'll
    generate actual code *)
  let the_module = L.create_module context "Neo" in
  (* Add types to the context so we can use them in our LLVM code *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type   context
  and array_t    = L.array_type
  and pointer_t  = L.pointer_type in

  (* Convert Neo types to LLVM types *)
  let matrix_types = ref StringMap.empty in
  let rec matrix_t typ =
    let string_typ = A.string_of_typ typ in
    try StringMap.find string_typ !matrix_types
    with Not_found ->
      let matrix_type = L.named_struct_type context ("matrix_t_" ^ string_typ) in
      let ltype = ltype_of_typ typ in
      let _ = L.struct_set_body matrix_type [| pointer_t (pointer_t ltype) ; i32_t ; i32_t |] false in
      matrix_types := StringMap.add string_typ matrix_type !matrix_types;
      matrix_type

  and ltype_of_typ = function
      A.Int      -> i32_t
    | A.Bool     -> i1_t
    | A.Float    -> float_t
    | A.Void     -> void_t
    | A.Matrix t -> matrix_t t
    | _ -> make_err "not supported yet in ltype_of_typ"
  in

  (* Built-in functions *)
  let printf_t : L.lltype = L.var_arg_function_type i32_t [| pointer_t i8_t |] in
  let printf_func : L.llvalue = L.declare_function "printf" printf_t the_module in

  let printm_int_t = L.function_type i32_t [| pointer_t (matrix_t A.Int) |] in
  let printm_int_func = L.declare_function "printm_int" printm_int_t the_module in

  let typ_of_mat_typ = function
    A.Matrix(t) -> t
  | _ -> make_err "internal error: matrix sexpr should have matrix type"
  in

  (* Returns initial value for an empty declaration of a given type *)
  let init t = match t with
      A.Float -> L.const_float float_t 0.0
    | A.Int -> L.const_int i32_t 0
    | _ -> make_err "not supported yet in init"
  in

  (* Returns value of an identifier *)
  let rec lookup name scope =
    try StringMap.find name scope.variables
    with Not_found ->
      match scope.parent with
          None -> make_err "internal error: semant should have rejected on undeclared identifier"
        | Some parent -> lookup name parent
  in

  let add_func_decl scope fdecl =
    let fname = fdecl.sfname in
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
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

    (* TODO: support heap alloc (will be needed if we're returning matrices) *)
    let make_matrix typ raw_elements builder =
      let matrix_t = matrix_t typ in
      let ltype = ltype_of_typ typ in
      let rows = Array.length raw_elements in
      let cols = Array.length raw_elements.(0) in

      (* Allocate and fill matrix contents *)
      let raw_rows = Array.map (L.const_array ltype) raw_elements in
      let raw_matrix = L.const_array (array_t ltype cols) raw_rows in
      let body = L.build_alloca (array_t (array_t ltype cols) rows) "body" builder in
      let _ = L.build_store raw_matrix body builder in

      (* Allocate array of pointers for each row of body *)
      let row_ptrs = L.build_array_alloca (pointer_t ltype) (L.const_int i32_t rows) "row_ptrs" builder in
      let store_row_ptr i _ =
        let si = string_of_int i in
        let row_ptr =
          L.build_in_bounds_gep body [| L.const_int i32_t i ; L.const_int i32_t 0 |]
            ("row_ptr_" ^ si) builder
        in
        let row_ptr =
          L.build_bitcast row_ptr (pointer_t ltype) ("row_ptr_bitcast_" ^ si) builder
        in
        let row = L.build_in_bounds_gep row_ptrs [| L.const_int i32_t i |] ("row_" ^ si) builder in
        let _ = L.build_store row_ptr row builder in
        ()
      in
      let _ = Array.iteri store_row_ptr raw_rows in

      (* Set body pointer as pointer to first row pointer *)
      let body_ptr = L.build_in_bounds_gep row_ptrs [| L.const_int i32_t 0 |] "body_ptr" builder in
      let body_ptr = L.build_bitcast body_ptr (pointer_t (pointer_t ltype)) "body_ptr_bitcast" builder in

      (* Allocate matrix *)
      let mat = L.build_alloca matrix_t "mat" builder in
      let mat_body = L.build_struct_gep mat 0 "mat_body" builder in
      let mat_rows = L.build_struct_gep mat 1 "mat_rows" builder in
      let mat_cols = L.build_struct_gep mat 2 "mat_cols" builder in

      (* Fill matrix fields *)
      let _ = L.build_store body_ptr mat_body builder in
      let _ = L.build_store (L.const_int i32_t rows) mat_rows builder in
      let _ = L.build_store (L.const_int i32_t cols) mat_cols builder in
      mat
    in

    match e with
        SInt_Lit i -> L.const_int i32_t i
      | SBool_Lit b -> L.const_int i1_t (if b then 1 else 0)
      | SFloat_Lit l -> L.const_float_of_string float_t l
      | SMatrix_Lit l ->
          let raw_elements = Array.map (Array.map (expr scope builder)) l in
          let typ = typ_of_mat_typ t in
          make_matrix typ raw_elements builder
      | SNoexpr -> init t
      | SId s -> L.build_load (lookup s scope) s builder
      | SAssign(e1, _, e2) ->
          let s =
            let _, e1 = e1 in
            match e1 with
                SId s -> s
              | _ -> make_err "not yet supported in expr"
          in
          let e' = expr scope builder e2 in (* TODO: handle ops *)
          let _  = L.build_store e' (lookup s scope) builder in e'
      | SBinop(e1, op, e2) ->
          let (t, _) = e1
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
      | SCall("print", [e]) -> (
            match t with
                A.Int | A.Bool -> L.build_call printf_func
                  [| int_format_str ; (expr scope builder e) |] "printf" builder
              | A.Matrix typ -> (
                    let m = expr scope builder e in
                    match typ with
                        A.Int -> L.build_call printm_int_func [| m |] "printm_int" builder
                      | _ -> make_err "not supported yet in print (matrix)"
                  )
              | _ -> make_err "not supported yet in print"
          )
      | SCall("printf", [e]) ->
          L.build_call printf_func [| float_format_str ; (expr scope builder e) |]
          "printf" builder
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
    let rec stmt scope builder = function
        SBlock sl ->
          let scope = { variables = StringMap.empty ; parent = Some scope } in
          List.fold_left (stmt scope) builder sl
        (* Generate code for this expression, return resulting builder *)
      | SExpr e -> let _ = expr scope builder e in builder
      | SReturn e ->
          let _ =
            match fdecl.styp with
                (* Special "return nothing" instr *)
                A.Void -> L.build_ret_void builder
                (* Build return statement *)
              | _ -> L.build_ret (expr scope builder e) builder
          in
          builder
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
          let then_builder = stmt scope (L.builder_at_end context then_bb) then_stmt in
          (* Add a branch to the "then" block (to the merge block)
            if a terminator doesn't already exist for the "then" block *)
          let () = add_terminal then_builder branch_instr in
          (* Identical to stuff we did for "then" *)
          let else_bb = L.append_block context "else" the_function in
          let else_builder = stmt scope (L.builder_at_end context else_bb) else_stmt in
          let () = add_terminal else_builder branch_instr in
          (* Generate initial branch instruction perform the selection of "then"
          or "else". Note we're using the builder we had access to at the start
          of this alternative. *)
          let _ = L.build_cond_br bool_val then_bb else_bb builder in
          (* Move to the merge block for further instruction building *)
          L.builder_at_end context merge_bb

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
          let while_builder = stmt scope (L.builder_at_end context body_bb) body in
          let () = add_terminal while_builder (L.build_br pred_bb) in
          (* Generate the predicate code in the predicate block *)
          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val = expr scope pred_builder predicate in
          (* Hook everything up *)
          let merge_bb = L.append_block context "merge" the_function in
          let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
          L.builder_at_end context merge_bb

      (* Implement for loops as while loops! *)
      | SFor(e1, e2, e3, body) ->
          stmt scope builder (SBlock [SExpr e1 ; SWhile(e2, SBlock [body ; SExpr e3])])
      | _ -> make_err "not supported yet in stmt"
    in

    (* Build the code for each statement in the function *)
    let builder = stmt scope builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  let global_scope =
    let global_scope = { variables = StringMap.empty ; parent = None } in
    let add_global_decl scope sdecl =
      (* TODO: support initializers too; we can maybe do this by renaming the
       * program "main" function to "program_main" and creating a "true"
       * main function that assigns the globals and calls "program_main" *)
      let _, t, s, _ = sdecl in
      let init = init t in
      let global = L.define_global s init the_module in
      { scope with variables = StringMap.add s global scope.variables }
    in
    let global_scope = List.fold_left add_global_decl global_scope globals in
    List.fold_left add_func_decl global_scope functions
  in
  List.iter (build_function_body global_scope) functions;
  the_module
