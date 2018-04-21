(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

(* We'll refer to Llvm and Ast constructs with module names *)
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
let translate (env, program) =
  let array_types = env.S.array_types in
  let func_types = env.S.func_types in
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

  let matrix_t =
    let matrix_type = L.named_struct_type context "matrix_t" in
    let body_type = pointer_t (pointer_t float_t) in
    (* Struct: double pointer to first element, nrows, ncols, element type *)
    let _ = L.struct_set_body matrix_type [| body_type; i32_t; i32_t; i32_t |] false in
    matrix_type
  in

  let array_t =
    let array_type = L.named_struct_type context "array_t" in
    let body_type = pointer_t (pointer_t i8_t) in
    (* Struct: void pointer to first element, length, size of elements *)
    let _ = L.struct_set_body array_type [| body_type; i32_t; i64_t |] false in
    array_type
  in

  let slice_t =
    let slice_type = L.named_struct_type context "slice_t" in
    let _ = L.struct_set_body slice_type [| i32_t; i32_t |] false in
    slice_type
  in

  let rec ltype_of_typ = function
      A.Int      -> i32_t
    | A.Bool     -> i1_t
    | A.Float    -> float_t
    | A.Void     -> void_t
    | A.String   -> pointer_t i8_t
    | A.Array _  -> pointer_t array_t
    | A.Matrix _ -> pointer_t matrix_t
    | A.Func(arg_types, ret_type) ->
        let func_t =
          L.function_type (ltype_of_typ ret_type)
          (Array.of_list (List.map ltype_of_typ arg_types))
        in
        pointer_t func_t
    | A.BuiltInFunc ->
        make_err ("internal error: BuiltInFunc should not have made " ^
        "it past semant (ltype_of_typ)")
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
  let main_ptr = L.define_global "main_ptr" main the_module in
  let main_builder = L.builder_at_end context (L.entry_block main) in

  (* Built-in format strings *)
  let empty_str = L.build_global_stringptr "" "empty_string" main_builder in
  let newline_str = L.build_global_stringptr "\n" "newline_string" main_builder in

  (* Declare built-in functions *)
  (* Pretty-printing functions *)
  let _print_bool_t = L.function_type void_t [| i1_t |] in
  let _print_bool_func = L.declare_function "_print_bool" _print_bool_t the_module in

  let _print_int_t = L.function_type void_t [| i32_t |] in
  let _print_int_func = L.declare_function "_print_int" _print_int_t the_module in

  let _print_string_t = L.function_type void_t [| pointer_t i8_t |] in
  let _print_string_func = L.declare_function "_print_string" _print_string_t the_module in

  let _print_float_t = L.function_type void_t [| float_t |] in
  let _print_float_func = L.declare_function "_print_float" _print_float_t the_module in

  let _print_matrix_t = L.function_type void_t [| pointer_t matrix_t |] in
  let _print_matrix_func = L.declare_function "_print_matrix" _print_matrix_t the_module in

  let print_function_t = L.function_type void_t [| pointer_t i8_t |] in
  let print_function_func = L.declare_function "print_function" print_function_t the_module in

  let print_element_t = L.function_type void_t [| pointer_t i8_t |] in
  let print_array_t =
    L.function_type void_t [| pointer_t array_t; pointer_t print_element_t |]
  in
  let print_array_func = L.declare_function "print_array" print_array_t the_module in

  let print_matrix_t = L.function_type void_t [| pointer_t matrix_t; i1_t |] in
  let print_matrix_func = L.declare_function "print_matrix" print_matrix_t the_module in

  (* Array/matrix memory functions *)
  let _free_matrix_t = L.function_type void_t [| pointer_t matrix_t |] in
  let _free_matrix_func = L.declare_function "_free_matrix" _free_matrix_t the_module in

  let _free_array_t = L.function_type void_t [| pointer_t array_t |] in
  let _free_array_func = L.declare_function "_free_array" _free_array_t the_module in

  let free_element_t = L.function_type void_t [| pointer_t i8_t |] in
  let deep_free_array_t =
    L.function_type void_t [| pointer_t array_t; pointer_t free_element_t |]
  in
  let deep_free_array_func = L.declare_function "deep_free_array" deep_free_array_t the_module in

  let set_ptrs_matrix_t = L.function_type void_t [| pointer_t matrix_t; pointer_t i8_t |] in
  let set_ptrs_matrix_func = L.declare_function "set_ptrs_matrix" set_ptrs_matrix_t the_module in

  let set_ptrs_array_t = L.function_type void_t [| pointer_t array_t; pointer_t i8_t |] in
  let set_ptrs_array_func = L.declare_function "set_ptrs_array" set_ptrs_array_t the_module in

  (* Array index/slice functions *)
  let get_array_t = L.function_type (pointer_t i8_t) [| pointer_t array_t; i32_t |] in
  let get_array_func = L.declare_function "get_array" get_array_t the_module in

  let set_array_t = L.function_type void_t [| pointer_t array_t; i32_t; pointer_t i8_t |] in
  let set_array_func = L.declare_function "set_array" set_array_t the_module in

  let slice_array_t =
    L.function_type void_t
    [| pointer_t array_t; pointer_t slice_t; pointer_t array_t |]
  in
  let slice_array_func = L.declare_function "slice_array" slice_array_t the_module in

  let set_slice_array_t =
    L.function_type void_t
    [| pointer_t array_t; pointer_t slice_t; pointer_t array_t |]
  in
  let set_slice_array_func = L.declare_function "set_slice_array" set_slice_array_t the_module in

  (* Matrix index/slice functions *)
  let get_matrix_t = L.function_type (pointer_t i8_t) [| pointer_t matrix_t; i32_t; i32_t |] in
  let get_matrix_func = L.declare_function "get_matrix" get_matrix_t the_module in

  let set_matrix_t = L.function_type void_t [| pointer_t matrix_t; i32_t; i32_t; pointer_t i8_t |] in
  let set_matrix_func = L.declare_function "set_matrix" set_matrix_t the_module in

  let slice_matrix_t =
    L.function_type void_t
    [| pointer_t matrix_t; pointer_t slice_t; pointer_t slice_t; pointer_t matrix_t |]
  in
  let slice_matrix_func = L.declare_function "slice_matrix" slice_matrix_t the_module in

  let set_slice_matrix_t =
    L.function_type void_t
    [| pointer_t matrix_t; pointer_t slice_t; pointer_t slice_t; pointer_t matrix_t |]
  in
  let set_slice_matrix_func = L.declare_function "set_slice_matrix" set_slice_matrix_t the_module in

  (* Binary operations *)
  let iexp_t = L.function_type i32_t [| i32_t; i32_t |] in
  let iexp_func = L.declare_function "iexp" iexp_t the_module in

  let fexp_t = L.function_type float_t [| float_t; float_t |] in
  let fexp_func = L.declare_function "fexp" fexp_t the_module in

  let matmult_t = L.function_type void_t [| pointer_t matrix_t; pointer_t matrix_t; pointer_t matrix_t |] in
  let matmult_func = L.declare_function "matmult" matmult_t the_module in

  let mat_binop_t =
    L.function_type void_t
    [| pointer_t matrix_t; i32_t; pointer_t matrix_t; pointer_t matrix_t |]
  in
  let mat_binop_func = L.declare_function "mat_binop" mat_binop_t the_module in

  (* Miscellaneous helper/built-ins *)
  let init_matrix_t = L.function_type void_t [| pointer_t matrix_t |] in
  let init_matrix_func = L.declare_function "init_matrix" init_matrix_t the_module in

  let length_t = L.function_type i32_t [| pointer_t array_t |] in
  let length_func = L.declare_function "length" length_t the_module in

  let rows_t = L.function_type i32_t [| pointer_t matrix_t |] in
  let rows_func = L.declare_function "rows" rows_t the_module in

  let cols_t = L.function_type i32_t [| pointer_t matrix_t |] in
  let cols_func = L.declare_function "cols" cols_t the_module in

  let to_int_t = L.function_type i32_t [| float_t |] in
  let to_int_func = L.declare_function "to_int" to_int_t the_module in

  let to_float_t = L.function_type float_t [| i32_t |] in
  let to_float_func = L.declare_function "to_float" to_float_t the_module in

  (* Collect native print/free functions *)
  let print_funcs =
    let print_funcs = StringMap.empty in
    let print_funcs = StringMap.add "_print_int" _print_int_func print_funcs in
    let print_funcs = StringMap.add "_print_bool" _print_bool_func print_funcs in
    let print_funcs = StringMap.add "_print_float" _print_float_func print_funcs in
    let print_funcs = StringMap.add "_print_string" _print_string_func print_funcs in
    let print_funcs = StringMap.add "_print_matrix" _print_matrix_func print_funcs in
    print_funcs
  in

  let free_funcs =
    let free_funcs = StringMap.empty in
    let free_funcs = StringMap.add "_free_matrix" _free_matrix_func free_funcs in
    let free_funcs = StringMap.add "_free_array" _free_array_func free_funcs in
    free_funcs
  in

  (* Build any necessary element-printing functions for array types *)
  let rec build_print_element_func array_type print_element_funcs =
    let typ = A.typ_of_container array_type in
    if TypeMap.mem array_type print_element_funcs then print_element_funcs
    else
      let print_t = L.function_type void_t [| pointer_t i8_t |] in
      let print_func = L.define_function ("print_" ^ A.string_of_typ typ) print_t the_module in
      let builder = L.builder_at_end context (L.entry_block print_func) in
      let ltype = ltype_of_typ typ in

      (* Cast and load param *)
      let param_ptr = (L.params print_func).(0) in
      let param_ptr = L.build_bitcast param_ptr (pointer_t ltype) "param_ptr" builder in
      let param = L.build_load param_ptr "param" builder in

      (* Call corresponding print function; if we have another array, recursively make it *)
      let print_element_funcs = match typ with
          A.Array _ -> build_print_element_func typ print_element_funcs
        | _ -> print_element_funcs
      in
      let _ = match typ with
          A.Int -> L.build_call _print_int_func [| param |] "" builder
        | A.Bool -> L.build_call _print_bool_func [| param |] "" builder
        | A.Float -> L.build_call _print_float_func [| param |] "" builder
        | A.String -> L.build_call _print_string_func [| param |] "" builder
        | A.Array _ ->
            L.build_call print_array_func
            [| param; TypeMap.find typ print_element_funcs |] "" builder
        | A.Matrix _ ->
            (* Print flat matrices if embedded within arrays *)
            L.build_call print_matrix_func
            [| param; L.const_int i1_t 1 |] "" builder
        | A.Func(_, _) ->
            (* Cast function pointer to "void pointer" *)
            let param = L.build_bitcast param (pointer_t i8_t) "param" builder in
            L.build_call print_function_func [| param |] "" builder
        | A.BuiltInFunc ->
            make_err ("internal error: BuiltInFunc should have not made it " ^
            "past semant (build print)")
        | A.Void -> make_err "internal error: semant should have rejected void data (build print)"
        | _ -> make_err "not supported yet in print"
      in
      let _ = L.build_ret_void builder in
      TypeMap.add array_type print_func print_element_funcs
  in
  let print_element_funcs = S.TypeSet.fold build_print_element_func array_types TypeMap.empty in

  (* Build any necessary single-argument array-printing functions *)
  let build_print_array_func array_type print_array_funcs =
    let fname = "_print_" ^ A.string_of_typ array_type in
    let print_t = L.function_type void_t [| ltype_of_typ array_type |] in
    let print_func = L.define_function fname print_t the_module in
    let builder = L.builder_at_end context (L.entry_block print_func) in

    (* Get param *)
    let param = (L.params print_func).(0) in

    (* Call print_array *)
    let _ =
      L.build_call print_array_func
      [| param; TypeMap.find array_type print_element_funcs |] "" builder
    in
    let _ = L.build_ret_void builder in
    StringMap.add fname print_func print_array_funcs
  in
  let print_funcs = S.TypeSet.fold build_print_array_func array_types print_funcs in

  (* Build any necessary function-printing functions *)
  let build_print_function_func func_type print_function_funcs =
    let fname = "_print_" ^ A.string_of_typ func_type in
    let print_t = L.function_type void_t [| ltype_of_typ func_type |] in
    let print_func = L.define_function fname print_t the_module in
    let builder = L.builder_at_end context (L.entry_block print_func) in

    (* Get param *)
    let param = (L.params print_func).(0) in

    (* Cast function pointer to "void pointer" *)
    let param = L.build_bitcast param (pointer_t i8_t) "param" builder in
    let _ = L.build_call print_function_func [| param |] "" builder in
    let _ = L.build_ret_void builder in
    StringMap.add fname print_func print_function_funcs
  in
  let print_funcs = S.TypeSet.fold build_print_function_func func_types print_funcs in

  (* Build println functions; note that all necessary print functions should be built
   * at this point; it takes each print function and makes a corresponding println
   * variant *)
  let build_println_func print_fname print_func println_funcs =
    let regexp = Str.regexp "_print" in
    let println_fname = Str.replace_first regexp "_println" print_fname in
    (* Note that print_func is actually a function pointer, so we need to get the
     * "dereferenced" type *)
    let println_t = L.element_type (L.type_of print_func) in
    let println_func = L.define_function println_fname println_t the_module in
    let builder = L.builder_at_end context (L.entry_block println_func) in

    (* Get param *)
    let param = (L.params println_func).(0) in

    (* First print param, then print newline *)
    let _ = L.build_call print_func [| param |] "" builder in
    let _ = L.build_call _print_string_func [| newline_str |] "" builder in
    let _ = L.build_ret_void builder in
    StringMap.add println_fname println_func println_funcs
  in
  let println_funcs = StringMap.fold build_println_func print_funcs StringMap.empty in

  (* Build any necessary element-freeing functions for array types *)
  let rec build_free_element_func array_type free_element_funcs =
    let typ = A.typ_of_container array_type in
    if TypeMap.mem array_type free_element_funcs then free_element_funcs
    else
      let free_t = L.function_type void_t [| pointer_t i8_t |] in
      let free_func =
        L.define_function ("free_" ^ A.string_of_typ typ) free_t the_module
      in
      let builder = L.builder_at_end context (L.entry_block free_func) in
      let ltype = ltype_of_typ typ in

      (* Cast and load param *)
      let param_ptr = (L.params free_func).(0) in
      let param_ptr = L.build_bitcast param_ptr (pointer_t ltype) "param_ptr" builder in
      let param = L.build_load param_ptr "param" builder in

      (* Perform corresponding free action; if we have another array,
       * recursively make it *)
      let free_element_funcs = match typ with
          A.Array _ -> build_free_element_func typ free_element_funcs
        | _ -> free_element_funcs
      in
      let _ = match typ with
          A.Array _ ->
            let _ =
              L.build_call deep_free_array_func
              [| param; TypeMap.find typ free_element_funcs |] "" builder
            in
            ()
        | A.Matrix _ ->
            let _ =
              L.build_call _free_matrix_func [| param |] "" builder
            in
            ()
        (* Otherwise, do nothing *)
        | _ -> ()
      in
      let _ = L.build_ret_void builder in
      TypeMap.add array_type free_func free_element_funcs
  in
  let free_element_funcs = S.TypeSet.fold build_free_element_func array_types TypeMap.empty in

  (* Build any necessary single-argument array deep-freeing functions *)
  let build_deep_free_func array_type deep_free_funcs =
    let fname = "_deep_free_" ^ A.string_of_typ array_type in
    let deep_free_t = L.function_type void_t [| ltype_of_typ array_type |] in
    let deep_free_func =
      L.define_function fname deep_free_t the_module
    in
    let builder = L.builder_at_end context (L.entry_block deep_free_func) in

    (* Cast and load param *)
    let param = (L.params deep_free_func).(0) in

    (* Call deep_free_array *)
    let _ =
      L.build_call deep_free_array_func
      [| param; TypeMap.find array_type free_element_funcs |] "" builder
    in
    let _ = L.build_ret_void builder in
    StringMap.add fname deep_free_func deep_free_funcs
  in
  let deep_free_funcs = S.TypeSet.fold build_deep_free_func array_types StringMap.empty in

  (* Returns initial value for an empty declaration of a given type *)
  let init t = match t with
      A.Float -> L.const_float float_t 0.0
    | A.Int -> L.const_int i32_t 0
    | A.Bool -> L.const_int i1_t 0
    | A.String -> empty_str
    | A.Array _ -> L.const_null (ltype_of_typ t)
    | A.Matrix _ -> L.const_null (ltype_of_typ t)
    | A.Func(_, _) -> L.const_null (ltype_of_typ t)
    | A.BuiltInFunc ->
        make_err ("internal error: BuiltInFunc should have not made it " ^
        "past semant (init)")
    | A.Void -> make_err "internal error: semant should have rejected void data (init)"
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
          None -> make_err ("internal error: semant should have rejected on undeclared identifier " ^ name)
        | Some parent -> lookup name parent
  in

  let add_func_decl (scope, functions) fdecl =
    let fname = fdecl.sfname in
    let return_type = fdecl.styp in
    let get_decl_type (_, typ, _, _) = typ in
    let arg_types = List.map get_decl_type fdecl.sformals in
    let arg_types = Array.of_list (List.map ltype_of_typ arg_types) in
    let typ = L.function_type (ltype_of_typ return_type) arg_types in
    let f = L.define_function fname typ the_module in
    let f_ptr = L.define_global (fname ^ "_ptr") f the_module in
    (
      { scope with variables = StringMap.add fname f_ptr scope.variables },
      StringMap.add fname f functions
    )
  in

  (* Fill in the body of the given function *)
  let build_function_body scope builder the_function fdecl =
    (* Construct code for an expression; return its value *)
    let rec expr scope builder (t, e) =
      let build_array typ length builder =
        let ltype = ltype_of_typ typ in
        (* Allocate required space *)
        let body_ptr = L.build_array_malloc (pointer_t i8_t) length "body_ptr" builder in
        let arr_ptr = L.build_malloc array_t "arr_ptr" builder in

        (* Set fields *)
        let arr_body = L.build_struct_gep arr_ptr 0 "arr_body" builder in
        let arr_length = L.build_struct_gep arr_ptr 1 "arr_length" builder in
        let arr_size = L.build_struct_gep arr_ptr 2 "arr_size" builder in
        let _ = L.build_store body_ptr arr_body builder in
        let _ = L.build_store length arr_length builder in
        let _ = L.build_store (L.size_of ltype) arr_size builder in
        arr_ptr
      in

      let build_array_lit typ raw_elements builder =
        let ltype = ltype_of_typ typ in
        let length = L.const_int i32_t (Array.length raw_elements) in
        let arr_ptr = build_array typ length builder in

        (* Allocate and fill array body *)
        let body = L.build_array_malloc ltype length "body" builder in
        let set_element i element =
          let ptr =
            L.build_in_bounds_gep body
            [| L.const_int i32_t i |] "ptr" builder
          in
          let _ = L.build_store element ptr builder in
          ()
        in
        let _ = Array.iteri set_element raw_elements in

        (* Cast body to "void pointer" *)
        let body = L.build_bitcast body (pointer_t i8_t) "body" builder in

        (* Set body pointers *)
        let _ = L.build_call set_ptrs_array_func [| arr_ptr; body |] "" builder in
        arr_ptr
      in

      let build_empty_array typ length builder =
        let ltype = ltype_of_typ typ in
        let arr_ptr = build_array typ length builder in

        (* Allocate array contents *)
        let body = L.build_array_malloc ltype length "body" builder in

        (* Cast body to "void pointer" *)
        let body = L.build_bitcast body (pointer_t i8_t) "body" builder in

        (* Set body pointers *)
        let _ = L.build_call set_ptrs_array_func [| arr_ptr; body |] "" builder in
        arr_ptr
      in

      let get_mat_type = function
          A.Int -> L.const_int i32_t 0
        (* Otherwise, it's A.Float *)
        | _ -> L.const_int i32_t 1
      in

      let build_matrix ?on_heap:(on_heap=true) typ rows cols builder =
        (* Get allocating functions *)
        let array_alloc = if on_heap then L.build_array_malloc else L.build_array_alloca in
        let alloc = if on_heap then L.build_malloc else L.build_alloca in

        (* Allocate required space *)
        let row_ptrs = array_alloc (pointer_t float_t) rows "row_ptrs" builder in
        let mat_ptr = alloc matrix_t "mat_ptr" builder in

        (* Set fields *)
        let mat_body = L.build_struct_gep mat_ptr 0 "mat_body" builder in
        let mat_rows = L.build_struct_gep mat_ptr 1 "mat_rows" builder in
        let mat_cols = L.build_struct_gep mat_ptr 2 "mat_cols" builder in
        let mat_type = L.build_struct_gep mat_ptr 3 "mat_type" builder in
        let _ = L.build_store row_ptrs mat_body builder in
        let _ = L.build_store rows mat_rows builder in
        let _ = L.build_store cols mat_cols builder in
        let _ = L.build_store (get_mat_type typ) mat_type builder in
        mat_ptr
      in

      let build_matrix_lit ?on_heap:(on_heap=true) typ raw_elements builder =
        (* Get allocating function *)
        let array_alloc = if on_heap then L.build_array_malloc else L.build_array_alloca in

        let ltype = ltype_of_typ typ in
        let rows = L.const_int i32_t (Array.length raw_elements) in
        let cols = L.const_int i32_t (Array.length raw_elements.(0)) in
        let size = L.build_mul rows cols "size" builder in
        let mat_ptr = build_matrix ~on_heap:on_heap typ rows cols builder in

        (* Flatten elements into one array *)
        let raw_elements = Array.concat (Array.to_list raw_elements) in

        (* Allocate and fill matrix body *)
        let body_ptr = array_alloc ltype size "body_ptr" builder in
        let set_element i element =
          let ptr =
            L.build_in_bounds_gep body_ptr
            [| L.const_int i32_t i |] "ptr" builder
          in
          let _ = L.build_store element ptr builder in
          ()
        in
        let _ = Array.iteri set_element raw_elements in

        (* Cast body ptr to "void pointer" *)
        let body_ptr = L.build_bitcast body_ptr (pointer_t i8_t) "body_ptr" builder in

        (* Set body pointers *)
        let _ = L.build_call set_ptrs_matrix_func [| mat_ptr; body_ptr |] "" builder in
        mat_ptr
      in

      let build_empty_matrix typ rows cols builder =
        let ltype = ltype_of_typ typ in
        let size = L.build_mul rows cols "size" builder in
        let mat_ptr = build_matrix typ rows cols builder in

        (* Allocate matrix body *)
        let body_ptr = L.build_array_malloc ltype size "body_ptr" builder in

        (* Cast body ptr to "void pointer" *)
        let body_ptr = L.build_bitcast body_ptr (pointer_t i8_t) "body_ptr" builder in

        (* Set body pointers *)
        let _ = L.build_call set_ptrs_matrix_func [| mat_ptr; body_ptr |] "" builder in
        mat_ptr
      in

      let get_rows mat builder =
        let rows_ptr = L.build_struct_gep mat 1 "rows_ptr" builder in
        L.build_load rows_ptr "rows" builder
      in

      let get_cols mat builder =
        let cols_ptr = L.build_struct_gep mat 2 "cols_ptr" builder in
        L.build_load cols_ptr "cols" builder
      in

      let build_sgl_slice arr slice builder = match slice with
          SSlice(i, j) ->
            let slice_ptr = L.build_alloca slice_t "slice_ptr" builder in
            let slice_start = L.build_struct_gep slice_ptr 0 "slice_start" builder in
            let slice_end = L.build_struct_gep slice_ptr 1 "slice_end" builder in
            let i' = expr scope builder i in
            let j' =
              match snd j with
                  SInt_Lit _ -> expr scope builder j
                (* Otherwise, it's SEnd *)
                | _ ->
                      let length_ptr = L.build_struct_gep arr 1 "length_ptr" builder in
                    L.build_load length_ptr "length" builder
            in
            let _ = L.build_store i' slice_start builder in
            let _ = L.build_store j' slice_end builder in
            slice_ptr
        | _ -> make_err "internal error: build_sgl_slice given non-slice"
      in

      let build_dbl_slice mat row_slice col_slice builder = match (row_slice, col_slice) with
          (SSlice(i1, j1), SSlice(i2, j2)) ->
            let row_slice_ptr = L.build_alloca slice_t "row_slice_ptr" builder in
            let row_slice_start = L.build_struct_gep row_slice_ptr 0 "row_slice_start" builder in
            let row_slice_end = L.build_struct_gep row_slice_ptr 1 "row_slice_end" builder in
            let col_slice_ptr = L.build_alloca slice_t "col_slice_ptr" builder in
            let col_slice_start = L.build_struct_gep col_slice_ptr 0 "col_slice_start" builder in
            let col_slice_end = L.build_struct_gep col_slice_ptr 1 "col_slice_end" builder in
            let i1' = expr scope builder i1 in
            let j1' =
              match snd j1 with
                  SInt_Lit _ -> expr scope builder j1
                | SSlice_Inc -> L.build_add i1' (L.const_int i32_t 1) "row_slice_inc" builder
                (* Otherwise, it's SEnd *)
                | _ -> get_rows mat builder
            in
            let i2' = expr scope builder i2 in
            let j2' =
              match snd j2 with
                  SInt_Lit _ -> expr scope builder j2
                | SSlice_Inc -> L.build_add i2' (L.const_int i32_t 1) "col_slice_inc" builder
                (* Otherwise, it's SEnd *)
                | _ -> get_cols mat builder
            in
            let _ = L.build_store i1' row_slice_start builder in
            let _ = L.build_store j1' row_slice_end builder in
            let _ = L.build_store i2' col_slice_start builder in
            let _ = L.build_store j2' col_slice_end builder in
            (row_slice_ptr, col_slice_ptr)
        | (_, _) -> make_err "internal error: build_dbl_slice given non-slice"
      in

      let build_slice_delta slice builder =
        let slice_start_ptr = L.build_struct_gep slice 0 "slice_start_ptr" builder in
        let slice_end_ptr = L.build_struct_gep slice 1 "slice_end_ptr" builder in
        let slice_start = L.build_load slice_start_ptr "slice_start" builder in
        let slice_end = L.build_load slice_end_ptr "slice_end" builder in
        L.build_sub slice_end slice_start "delta" builder
      in

      let build_slice_array typ slice builder =
        let length = build_slice_delta slice builder in
        build_empty_array typ length builder
      in

      let build_slice_matrix typ row_slice col_slice builder =
        let rows = build_slice_delta row_slice builder in
        let cols = build_slice_delta col_slice builder in
        build_empty_matrix typ rows cols builder
      in

      let sexpr_of_sindex = function
          SIndex e -> e
        | _ -> make_err "internal error: sexpr_of_sindex given non-index"
      in

      (* Used for element-wise matrix operations *)
      let get_mat_opcode op =
        match op with
            A.Add     -> 0
          | A.Sub     -> 1
          | A.Mult    -> 2
          | A.Div     -> 3
          | A.Mod     -> 4
          | A.Exp     -> 5
          | A.Equal   -> 6
          | A.Neq     -> 7
          | A.Less    -> 8
          | A.Leq     -> 9
          | A.Greater -> 10
          | A.Geq     -> 11
          | _       -> make_err (
              "internal error: op " ^ A.string_of_op op ^
              " is not an element-wise matrix op"
            )
      in

      (* TODO: perform runtime checks; division by zero, index out of bounds,
      * working with null arrays/matrices (i.e. global array/matrix that hasn't
      * been assigned a real value) *)
      match e with
          SInt_Lit i -> L.const_int i32_t i
        | SBool_Lit b -> L.const_int i1_t (if b then 1 else 0)
        | SFloat_Lit l -> L.const_float_of_string float_t l
        | SString_Lit s -> L.build_global_stringptr (Scanf.unescaped s) "str" builder
        | SArray_Lit l ->
            let raw_array = Array.map (expr scope builder) l in
            let typ = A.typ_of_container t in
            build_array_lit typ raw_array builder
        | SEmpty_Array(t, n) ->
            let length = expr scope builder n in
            build_empty_array t length builder
        | SMatrix_Lit l ->
            let raw_elements = Array.map (Array.map (expr scope builder)) l in
            let typ = A.typ_of_container t in
            build_matrix_lit typ raw_elements builder
        | SEmpty_Matrix(t, r, c) ->
            let rows = expr scope builder r in
            let cols = expr scope builder c in
            let mat_ptr = build_empty_matrix t rows cols builder in
            let _ = L.build_call init_matrix_func [| mat_ptr |] "" builder in
            mat_ptr
        | SIndex_Expr i ->
            (
              match i with
                  SSgl_Index(e, i) ->
                    let ltype = ltype_of_typ t in
                    let e' = expr scope builder e in
                    let i' = expr scope builder (sexpr_of_sindex i) in
                    let ptr = L.build_call get_array_func [| e'; i' |] "ptr" builder in
                    let ptr = L.build_bitcast ptr (pointer_t ltype) "ptr" builder in
                    L.build_load ptr "arr_element" builder
                | SDbl_Index(e, i, j) ->
                    let ltype = ltype_of_typ t in
                    let e' = expr scope builder e in
                    let i' = expr scope builder (sexpr_of_sindex i) in
                    let j' = expr scope builder (sexpr_of_sindex j) in
                    let ptr =
                      L.build_call get_matrix_func [| e'; i'; j' |] "mat_element" builder
                    in
                    let ptr = L.build_bitcast ptr (pointer_t ltype) "ptr" builder in
                    L.build_load ptr "mat_element" builder
            )
        | SSlice_Expr s ->
            let typ = A.typ_of_container t in
            (
              match s with
                  SSgl_Slice(e, s) ->
                    let e' = expr scope builder e in
                    let s' = build_sgl_slice e' s builder in
                    let res_ptr = build_slice_array typ s' builder in
                    let _ = L.build_call slice_array_func [| e'; s'; res_ptr |] "" builder in
                    res_ptr
                | SDbl_Slice(e, s1, s2) ->
                    let e' = expr scope builder e in
                    let s1', s2' = build_dbl_slice e' s1 s2 builder in
                    let res_ptr = build_slice_matrix typ s1' s2' builder in
                    let _ =
                      L.build_call slice_matrix_func [| e'; s1'; s2'; res_ptr |] "" builder
                    in
                    res_ptr
            )
        | SNoexpr -> init t
        | SId s -> L.build_load (lookup s scope) s builder
        | SAssign(e1, e2) ->
            let t, e1 = e1 in
            let e2' = expr scope builder e2 in
            let _ =
              match e1 with
                  SId s ->
                    let _ = L.build_store e2' (lookup s scope) builder in
                    ()
                | SIndex_Expr i ->
                    (
                      match i with
                          SSgl_Index(e, i) ->
                            let ltype = ltype_of_typ t in
                            let e' = expr scope builder e in
                            let i' = expr scope builder (sexpr_of_sindex i) in
                            let ptr = L.build_alloca ltype "ptr" builder in
                            let _ = L.build_store e2' ptr builder in
                            let ptr = L.build_bitcast ptr (pointer_t i8_t) "ptr" builder in
                            let _ = L.build_call set_array_func [| e'; i'; ptr |] "" builder in
                            ()
                        | SDbl_Index(e, i, j) ->
                            let ltype = ltype_of_typ t in
                            let e' = expr scope builder e in
                            let i' = expr scope builder (sexpr_of_sindex i) in
                            let j' = expr scope builder (sexpr_of_sindex j) in
                            let ptr = L.build_alloca ltype "ptr" builder in
                            let _ = L.build_store e2' ptr builder in
                            let ptr = L.build_bitcast ptr (pointer_t i8_t) "ptr" builder in
                            let _ =
                              L.build_call set_matrix_func
                              [| e'; i'; j'; ptr |] "" builder
                            in
                            ()
                    )
                | SSlice_Expr s ->
                  (
                    match s with
                        SSgl_Slice(e, s) ->
                          let e' = expr scope builder e in
                          let s' = build_sgl_slice e' s builder in
                          let _ =
                            L.build_call set_slice_array_func
                            [| e'; s'; e2' |] "" builder
                          in
                          ()
                      | SDbl_Slice(e, s1, s2) ->
                          let e' = expr scope builder e in
                          let s1', s2' = build_dbl_slice e' s1 s2 builder in
                          let _ =
                            L.build_call set_slice_matrix_func
                            [| e'; s1'; s2'; e2' |] "" builder
                          in
                          ()
                  )
                | _ -> make_err "internal error: semant should have rejected invalid assignment"
            in
            expr scope builder (t, e1)
        | SBinop(e1, op, e2) ->
            let err =
              "internal error: semant should have rejected " ^
              A.string_of_op op ^ " on " ^ A.string_of_typ t
            in
            let t1 = fst e1 and t2 = fst e2 in
            let e1' = expr scope builder e1
            and e2' = expr scope builder e2 in
            if t1 = A.Float && t2 = A.Float then
            (
              match op with
                  A.Add     -> L.build_fadd e1' e2' "temp" builder
                | A.Sub     -> L.build_fsub e1' e2' "temp" builder
                | A.Mult    -> L.build_fmul e1' e2' "temp" builder
                | A.Div     -> L.build_fdiv e1' e2' "temp" builder
                | A.Mod     -> L.build_frem e1' e2' "temp" builder
                | A.Exp     -> L.build_call fexp_func [| e1'; e2'|] "temp" builder
                | A.Equal   -> L.build_fcmp L.Fcmp.Oeq e1' e2' "temp" builder
                | A.Neq     -> L.build_fcmp L.Fcmp.One e1' e2' "temp" builder
                | A.Less    -> L.build_fcmp L.Fcmp.Olt e1' e2' "temp" builder
                | A.Leq     -> L.build_fcmp L.Fcmp.Ole e1' e2' "temp" builder
                | A.Greater -> L.build_fcmp L.Fcmp.Ogt e1' e2' "temp" builder
                | A.Geq     -> L.build_fcmp L.Fcmp.Oge e1' e2' "temp" builder
                | _         -> make_err err
            )
            else if t1 = A.Int && t2 = A.Int then
            (
              match op with
                  A.Add     -> L.build_add e1' e2' "temp" builder
                | A.Sub     -> L.build_sub e1' e2' "temp" builder
                | A.Mult    -> L.build_mul e1' e2' "temp" builder
                | A.Div     -> L.build_sdiv e1' e2' "temp" builder
                | A.Mod     -> L.build_srem e1' e2' "temp" builder
                | A.Exp     -> L.build_call iexp_func [| e1'; e2'|] "temp" builder
                | A.Equal   -> L.build_icmp L.Icmp.Eq e1' e2' "temp" builder
                | A.Neq     -> L.build_icmp L.Icmp.Ne e1' e2' "temp" builder
                | A.Less    -> L.build_icmp L.Icmp.Slt e1' e2' "temp" builder
                | A.Leq     -> L.build_icmp L.Icmp.Sle e1' e2' "temp" builder
                | A.Greater -> L.build_icmp L.Icmp.Sgt e1' e2' "temp" builder
                | A.Geq     -> L.build_icmp L.Icmp.Sge e1' e2' "temp" builder
                | _         -> make_err err
            )
            else if t1 = A.Bool && t2 = A.Bool then
            (
              match op with
                  A.And -> L.build_and e1' e2' "temp" builder
                | A.Or  -> L.build_or e1' e2' "temp" builder
                | _     -> make_err err
            )
            (* Otherwise, it's a matrix result (either int or float) *)
            else
              let t = A.typ_of_container t in
              (* Wrap broadcasted scalars into temp 1x1 matrices stored on the stack *)
              let e1' =
                if t1 == A.Int || t1 == A.Float
                then build_matrix_lit ~on_heap:false t1 [| [| e1' |] |] builder
                else e1'
              in
              let e2' =
                if t2 == A.Int || t2 == A.Float
                then build_matrix_lit ~on_heap:false t2 [| [| e2' |] |] builder
                else e2'
              in
              (
                match op with
                    A.MatMult ->
                      let rows = get_rows e1' builder in
                      let cols = get_cols e2' builder in
                      let mat = build_empty_matrix t rows cols builder in
                      let _ = L.build_call matmult_func [| e1'; e2'; mat|] "" builder in
                      mat
                  | A.And | A.Or -> make_err err
                  | _ ->
                      let max e1' e2' builder =
                        let cond = L.build_icmp L.Icmp.Sgt e1' e2' "max_cond" builder in
                        L.build_select cond e1' e2' "max_v" builder
                      in
                      let opcode = L.const_int i32_t (get_mat_opcode op) in
                      let rows_1 = get_rows e1' builder in
                      let cols_1 = get_cols e1' builder in
                      let rows_2 = get_rows e2' builder in
                      let cols_2 = get_cols e2' builder in
                      let rows = max rows_1 rows_2 builder in
                      let cols = max cols_1 cols_2 builder in
                      let mat = build_empty_matrix t rows cols builder in
                      let _ =
                        L.build_call mat_binop_func
                        [| e1'; opcode; e2'; mat |] ""  builder
                      in
                      mat
              )
        | SUnop(op, e) ->
            let t, _ = e in
            let e' = expr scope builder e in
            (match op with
                A.Neg when t = A.Float -> L.build_fneg
              | A.Neg                  -> L.build_neg
              | A.Not                  -> L.build_not) e' "tmp" builder
        | SCall(f, args) ->
            let f' = expr scope builder f in
            let args' = List.map (expr scope builder) args in
            let result = (match t with
                A.Void -> ""
              | _ -> "result")
            in
            L.build_call f' (Array.of_list args') result builder
        | SSlice_Inc -> make_err "internal error: SSlice_Inc should not be passed to expr"
        | SEnd -> make_err "internal error: SEnd should not be passed to expr"
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
          let scope = { variables = StringMap.empty; parent = Some parent_scope } in
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
          let _ = L.build_store e' local builder in
          let scope =
            { scope with variables = StringMap.add s local scope.variables }
          in
          (scope, builder)
    in

    (* If no builder is provided, make one *)
    let builder = match builder with
        None -> L.builder_at_end context (L.entry_block the_function)
      | Some builder -> builder
    in

    let scope =
      let scope = { variables = StringMap.empty; parent = Some scope } in
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

    (* Build the code for each statement in the function *)
    let _, builder = stmt (scope, builder) (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    match fdecl.styp with
        A.Void -> add_terminal builder L.build_ret_void
      (* Semant should have detected if there was a missing required return *)
      | _ -> ()
  in

  let get_main_decl globals =
    let get_assign sdecl =
      let _, t, s, e = sdecl in
      let s' = (t, SId s) in
      SExpr(t, SAssign(s', e))
    in
    (* Assign global variables in main *)
    let globals = List.map get_assign globals in
    {
      styp = A.Int;
      sfname = sys_main;
      sformals = [];
      sbody = globals @ [
          SExpr(A.Void, SCall((A.Func([], A.Void), SId "main"), []));
          SReturn (A.Int, SInt_Lit 0)
        ]
    }
  in

  let global_scope = { variables = StringMap.empty ; parent = None } in
  (* Add pointers to native functions to scope *)
  let global_scope =
    let add_native_funcs fname f scope =
      let f_ptr = L.define_global (fname ^ "_ptr") f the_module in
      { scope with variables = StringMap.add fname f_ptr scope.variables }
    in
    let global_scope = StringMap.fold add_native_funcs print_funcs global_scope in
    let global_scope = StringMap.fold add_native_funcs println_funcs global_scope in
    let global_scope = StringMap.fold add_native_funcs free_funcs global_scope in
    let global_scope = StringMap.fold add_native_funcs deep_free_funcs global_scope in
    let global_scope = add_native_funcs "length" length_func global_scope in
    let global_scope = add_native_funcs "rows" rows_func global_scope in
    let global_scope = add_native_funcs "cols" cols_func global_scope in
    let global_scope = add_native_funcs "to_int" to_int_func global_scope in
    let global_scope = add_native_funcs "to_float" to_float_func global_scope in
    global_scope
  in
  (* Add global variables to scope *)
  let global_scope =
    let add_global_decl scope sdecl =
      let _, t, s, _ = sdecl in
      let init = init t in
      let global = L.define_global s init the_module in
      { scope with variables = StringMap.add s global scope.variables }
    in
    List.fold_left add_global_decl global_scope globals
  in
  (* Add function declarations to scope and to function map; this map keeps track
   * of the actual function pointers, whereas the scope stores pointers to these pointers *)
  let global_scope, functions' =
    List.fold_left add_func_decl (global_scope, StringMap.empty) functions
  in
  let global_scope, functions' =
    (
      { global_scope with variables = StringMap.add sys_main main_ptr global_scope.variables },
      StringMap.add sys_main main functions'
    )
  in
  let main_decl = get_main_decl globals in
  let build_function_decl functions' scope builder fdecl =
    let the_function = StringMap.find fdecl.sfname functions' in
    build_function_body scope builder the_function fdecl
  in
  let _ = List.iter (build_function_decl functions' global_scope None) functions in
  let _ = build_function_decl functions' global_scope (Some main_builder) main_decl in
  the_module
