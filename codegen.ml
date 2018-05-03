(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

(* We'll refer to Llvm and Ast constructs with module names *)
module O = Optimize
module S = Semant
module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

type symbol_table = {
    variables : L.llvalue StringMap.t;
    parent : symbol_table option
  }

let make_err err = raise (Failure err)

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let translate (internal_uses, program) =
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
    (* Struct: void pointer to first element, length, size of elements, and if
     * the array contains pointers to other objects (arrays, matrices, functions);
     * this is needed for null pointer checks, since empty arrays are filled with
     * the default value. We need this, as empty arrays of ints are filled with 0s,
     * so we don't want to confuse this as actually being the null pointer. *)
    let _ = L.struct_set_body array_type [| body_type; i32_t; i64_t; i1_t |] false in
    array_type
  in

  let slice_t =
    let slice_type = L.named_struct_type context "slice_t" in
    let _ = L.struct_set_body slice_type [| i32_t; i32_t |] false in
    slice_type
  in

  let rec ltype_of_typ = function
    | A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Float -> float_t
    | A.Void -> void_t
    | A.String -> pointer_t i8_t
    | A.Array _ -> pointer_t array_t
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
    | A.Notyp -> make_err ("internal error: Notyp should not have made " ^
        "it past semant (ltype_of_typ)")
  in

  let mat_type_of_typ = function
    | A.Int -> L.const_int i32_t 0
    (* Otherwise, it's A.Float *)
    | _ -> L.const_int i32_t 1
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
  let null_value_err =
    L.build_global_stringptr "Null value error: attempted to read null value"
    "null_value_err" main_builder
  in
  let div_zero_err =
    L.build_global_stringptr "Zero division error: attempted to perform division or modulo by 0"
    "div_zero_err" main_builder
  in
  let exp_zero_err =
    L.build_global_stringptr "Zero division error: attempted to raise 0 to a negative power"
    "exp_zero_err" main_builder
  in
  let exp_neg_err =
    L.build_global_stringptr "Arithmetic error: attempted to raise negative number to a non-integer power"
    "exp_neg_err" main_builder
  in

  let add_external_func external_funcs (fname, ret_ltype, arg_ltypes) =
    let builtin_func_t = L.function_type ret_ltype arg_ltypes in
    StringMap.add fname builtin_func_t external_funcs
  in

  let external_builtin_types =
    List.fold_left add_external_func StringMap.empty [
      (* Pretty-printing functions *)
      ("_print_bool", void_t, [| i1_t |]);
      ("_print_int", void_t, [| i32_t |]);
      ("_print_string", void_t, [| pointer_t i8_t |]);
      ("_print_float", void_t, [| float_t |]);
      ("_print_matrix", void_t, [| pointer_t matrix_t |]);
      (* Array/matrix memory functions *)
      ("_free_matrix", void_t, [| pointer_t matrix_t |]);
      ("_free_array", void_t, [| pointer_t array_t |]);
      (* Array index/slice functions *)
      ("_delete_array", pointer_t array_t, [| pointer_t array_t; i32_t |]);
      (* Matrix index/slice functions *)
      ("_insert_matrix", pointer_t matrix_t, [| pointer_t matrix_t; i32_t; pointer_t matrix_t |]);
      ("_delete_matrix", pointer_t matrix_t, [| pointer_t matrix_t; i32_t |]);
      ("_append_matrix", pointer_t matrix_t, [| pointer_t matrix_t; pointer_t matrix_t |]);
      (* File I/O fuctions *)
      ("_write_mat", void_t, [| pointer_t matrix_t; pointer_t i8_t |]);
      (* Miscellaneous built-ins *)
      ("_length", i32_t, [| pointer_t array_t |]);
      ("_rows", i32_t, [| pointer_t matrix_t |]);
      ("_cols", i32_t, [| pointer_t matrix_t |]);
      ("_float_to_int", i32_t, [| float_t |]);
      ("_int_to_float", float_t, [| i32_t |]);
      ("_flip_matrix_type", pointer_t matrix_t, [| pointer_t matrix_t |]);
      ("_die", void_t, [| pointer_t i8_t |]);
    ]
  in

  let external_helper_types =
    List.fold_left add_external_func StringMap.empty [
      (* Pretty-printing functions *)
      ("_print_function", void_t, [| pointer_t i8_t |]);
      (
        "_print_array", void_t,
        [|
          pointer_t array_t;
          pointer_t (L.function_type void_t [| pointer_t i8_t |])
        |]
      );
      ("_print_flat_matrix", void_t, [| pointer_t matrix_t |]);
      (* Array/matrix memory functions *)
      (
        "_deep_free_array", void_t,
        [|
          pointer_t array_t;
          pointer_t (L.function_type void_t [| pointer_t i8_t |])
        |]
      );
      ("_malloc_array", pointer_t array_t, [| i32_t; i64_t; i1_t |]);
      ("_malloc_matrix", pointer_t matrix_t, [| i32_t; i32_t; i32_t |]);
      (* Array index/slice functions *)
      ("_get_array", pointer_t i8_t, [| pointer_t array_t; i32_t |]);
      ("_set_array", void_t, [| pointer_t array_t; i32_t; pointer_t i8_t |]);
      ("_slice_array", pointer_t array_t, [| pointer_t array_t; pointer_t slice_t |]);
      ("_set_slice_array", void_t, [| pointer_t array_t; pointer_t slice_t; pointer_t array_t |]);
      (* Matrix index/slice functions *)
      ("_get_matrix", pointer_t i8_t, [| pointer_t matrix_t; i32_t; i32_t |]);
      ("_set_matrix", void_t, [| pointer_t matrix_t; i32_t; i32_t; pointer_t i8_t |]);
      (
        "_slice_matrix", pointer_t matrix_t,
        [| pointer_t matrix_t; pointer_t slice_t; pointer_t slice_t |]
      );
      (
        "_set_slice_matrix", void_t,
        [| pointer_t matrix_t; pointer_t slice_t; pointer_t slice_t; pointer_t matrix_t |]
      );
      ("_insert_array", pointer_t array_t, [| pointer_t array_t; i32_t; pointer_t i8_t |]);
      ("_append_array", pointer_t array_t, [| pointer_t array_t; pointer_t i8_t |]);
      (* Binary operations *)
      ("_iexp", i32_t, [| i32_t; i32_t |]);
      ("_fexp", float_t, [| float_t; float_t |]);
      ("_matmult", pointer_t matrix_t, [| pointer_t matrix_t; pointer_t matrix_t |]);
      ("_mat_binop", pointer_t matrix_t, [| pointer_t matrix_t; i32_t; pointer_t matrix_t |]);
      (* File I/O functions *)
      ("_read_mat", pointer_t matrix_t, [| pointer_t i8_t; i32_t |]);
      (* Miscellaneous helper/built-ins *)
      ("_init_array", void_t, [| pointer_t array_t; pointer_t i8_t |]);
      ("_init_matrix", void_t, [| pointer_t matrix_t |]);
      ("_check", void_t, [| i1_t; pointer_t i8_t |]);
      ("llvm.floor.f64", float_t, [| float_t |]);
    ]
  in

  let add_used_helper fname _ uses =
    if StringSet.mem fname internal_uses then StringSet.add fname uses
    else uses
  in
  let helper_uses = StringMap.fold add_used_helper external_helper_types StringSet.empty in
  let builtin_uses = StringSet.diff internal_uses helper_uses in

  let helper_funcs =
    let helper_has_use fname _ = StringSet.mem fname helper_uses in
    let external_helper_types = StringMap.filter helper_has_use external_helper_types in
    let build_external_helper fname helper_t helper_funcs =
      let helper_func = L.declare_function fname helper_t the_module in
      StringMap.add fname helper_func helper_funcs
    in
    StringMap.fold build_external_helper external_helper_types StringMap.empty
  in

  let call_helper fname args s builder =
    (* TODO: remove this *)
    let _ = if not (StringMap.mem fname helper_funcs) then make_err fname in
    let helper_func = StringMap.find fname helper_funcs in
    L.build_call helper_func args s builder
  in

  let build_external_builtin ?alias:(alias=None) fname builtin_funcs =
    if StringMap.mem fname builtin_funcs then
      match alias with
      | None -> builtin_funcs
      | Some alias ->
          if StringMap.mem alias builtin_funcs then builtin_funcs
          else
            let func = StringMap.find fname builtin_funcs in
            StringMap.add alias func builtin_funcs
    else if StringMap.mem fname external_builtin_types then
      let func_t = StringMap.find fname external_builtin_types in
      let func = L.declare_function fname func_t the_module in
      let builtin_funcs = StringMap.add fname func builtin_funcs in
      match alias with
      | None -> builtin_funcs
      | Some alias -> StringMap.add alias func builtin_funcs
    else make_err (fname ^ " is not an external built-in")
  in

  let rec build_builtin fname builtin_funcs =
    (* Build any necessary element-printing functions for array types *)
    let rec build_print_element_func array_type builtin_funcs =
      let typ = A.typ_of_container array_type in
      let fname = "_print_element_" ^ A.string_of_typ typ in
      if StringMap.mem fname builtin_funcs then builtin_funcs
      else
        let print_t = L.function_type void_t [| pointer_t i8_t |] in
        let print_func = L.define_function fname print_t the_module in
        let builder = L.builder_at_end context (L.entry_block print_func) in
        let ltype = ltype_of_typ typ in

        (* Cast and load param *)
        let param_ptr = (L.params print_func).(0) in
        let param_ptr = L.build_bitcast param_ptr (pointer_t ltype) "param_ptr" builder in
        let param = L.build_load param_ptr "param" builder in

        (* Call corresponding print function; if we have another array, recursively make it *)
        let builtin_funcs =
          match typ with
          | A.Array _ -> build_print_element_func typ builtin_funcs
          | _ -> builtin_funcs
        in
        let builtin_funcs =
          match typ with
          | A.Int | A.Bool | A.Float | A.String ->
              let print_fname = "_print_" ^ A.string_of_typ typ in
              let builtin_funcs = build_external_builtin print_fname builtin_funcs in
              let element_print_func = StringMap.find print_fname builtin_funcs in
              let _ = L.build_call element_print_func [| param |] "" builder in
              builtin_funcs
          | A.Array t ->
              let element_fname = "_print_element_" ^ A.string_of_typ t in
              let _ =
                call_helper "_print_array"
                [| param; StringMap.find element_fname builtin_funcs |] "" builder
              in
              builtin_funcs
          | A.Matrix _ ->
              (* Print flat matrices if embedded within arrays *)
              let _ = call_helper "_print_flat_matrix" [| param |] "" builder in
              builtin_funcs
          | A.Func(_, _) ->
              (* Cast function pointer to "void pointer" *)
              let param = L.build_bitcast param (pointer_t i8_t) "param" builder in
              let _ = call_helper "_print_function" [| param |] "" builder in
              builtin_funcs
          | A.BuiltInFunc ->
              make_err ("internal error: BuiltInFunc should have not made it " ^
              "past semant (build print_element)")
          | A.Void -> make_err "internal error: semant should have rejected void (build print_element)"
          | A.Notyp ->
              make_err("internal error: Notyp should have not made it " ^
              "past semant (build print_element)")
        in
        let _ = L.build_ret_void builder in
        StringMap.add fname print_func builtin_funcs
    in

    (* Build any necessary array-printing functions *)
    let build_print_array_func array_type builtin_funcs =
      let fname = "_print_" ^ A.string_of_typ array_type in
      let print_t = L.function_type void_t [| ltype_of_typ array_type |] in
      let print_func = L.define_function fname print_t the_module in
      let builder = L.builder_at_end context (L.entry_block print_func) in

      (* Get param *)
      let param = (L.params print_func).(0) in

      (* Call print_array *)
      let typ = A.typ_of_container array_type in
      let element_fname = "_print_element_" ^ A.string_of_typ typ in
      let builtin_funcs = build_print_element_func array_type builtin_funcs in
      let print_element_func = StringMap.find element_fname builtin_funcs in
      let _ =
        call_helper "_print_array"
        [| param; print_element_func |] "" builder
      in
      let _ = L.build_ret_void builder in
      StringMap.add fname print_func builtin_funcs
    in

    (* Build any necessary function-printing functions *)
    let build_print_function_func func_type builtin_funcs =
      let fname = "_print_" ^ A.string_of_typ func_type in
      let print_t = L.function_type void_t [| ltype_of_typ func_type |] in
      let print_func = L.define_function fname print_t the_module in
      let builder = L.builder_at_end context (L.entry_block print_func) in

      (* Get param *)
      let param = (L.params print_func).(0) in

      (* Cast function pointer to "void pointer" *)
      let param = L.build_bitcast param (pointer_t i8_t) "param" builder in
      let _ = call_helper "_print_function" [| param |] "" builder in
      let _ = L.build_ret_void builder in
      StringMap.add fname print_func builtin_funcs
    in

    (* Build any necessary println functions *)
    let build_println_func arg_type builtin_funcs =
      let print_fname = "_print_" ^ A.string_of_typ arg_type in
      let builtin_funcs =
        if StringMap.mem print_fname builtin_funcs then builtin_funcs
        else build_builtin print_fname builtin_funcs
      in
      let print_func = StringMap.find print_fname builtin_funcs in

      let fname = "_println_" ^ A.string_of_typ arg_type in
      (* Note that print_func is actually a function pointer, so we need to get the
      * "dereferenced" type *)
      let println_t = L.element_type (L.type_of print_func) in
      let println_func = L.define_function fname println_t the_module in
      let builtin_funcs = build_external_builtin "_print_string" builtin_funcs in
      let print_string_func = StringMap.find "_print_string" builtin_funcs in
      let builder = L.builder_at_end context (L.entry_block println_func) in

      (* Get param *)
      let param = (L.params println_func).(0) in

      (* First print param, then print newline *)
      let _ = L.build_call print_func [| param |] "" builder in
      let _ = L.build_call print_string_func [| newline_str |] "" builder in
      let _ = L.build_ret_void builder in
      StringMap.add fname println_func builtin_funcs
    in

    (* Build any necessary element-freeing functions for array types *)
    let rec build_free_element_func array_type builtin_funcs =
      let typ = A.typ_of_container array_type in
      let fname = "_free_element_" ^ A.string_of_typ typ in
      if StringMap.mem fname builtin_funcs then builtin_funcs
      else
        let free_t = L.function_type void_t [| pointer_t i8_t |] in
        let free_func = L.define_function fname free_t the_module in
        let builder = L.builder_at_end context (L.entry_block free_func) in
        let ltype = ltype_of_typ typ in

        (* Cast and load param *)
        let param_ptr = (L.params free_func).(0) in
        let param_ptr = L.build_bitcast param_ptr (pointer_t ltype) "param_ptr" builder in
        let param = L.build_load param_ptr "param" builder in

        (* Perform corresponding free action; if we have another array,
         * recursively make it *)
        let builtin_funcs =
          match typ with
          | A.Array _ -> build_free_element_func typ builtin_funcs
          | _ -> builtin_funcs
        in
        let builtin_funcs =
          match typ with
          | A.Array t ->
              let element_fname = "_free_element_" ^ A.string_of_typ t in
              let free_element_func = StringMap.find element_fname builtin_funcs in
              let _ =
                call_helper "_deep_free_array"
                [| param; free_element_func |] "" builder
              in
              builtin_funcs
          | A.Matrix _ ->
              let builtin_funcs = build_external_builtin "_free_matrix" builtin_funcs in
              let free_matrix_func = StringMap.find "_free_matrix" builtin_funcs in
              let _ =
                L.build_call free_matrix_func [| param |] "" builder
              in
              builtin_funcs
          (* Otherwise, do nothing *)
          | _ -> builtin_funcs
        in
        let _ = L.build_ret_void builder in
        StringMap.add fname free_func builtin_funcs
    in

    (* Build any necessary array deep-freeing functions *)
    let build_deep_free_func array_type builtin_funcs =
      let fname = "_deep_free_" ^ A.string_of_typ array_type in
      let deep_free_t = L.function_type void_t [| ltype_of_typ array_type |] in
      let deep_free_func = L.define_function fname deep_free_t the_module in
      let builder = L.builder_at_end context (L.entry_block deep_free_func) in

      (* Get param *)
      let param = (L.params deep_free_func).(0) in

      (* Call deep_free_array *)
      let typ = A.typ_of_container array_type in
      let element_fname = "_free_element_" ^ A.string_of_typ typ in
      let builtin_funcs = build_free_element_func array_type builtin_funcs in
      let free_element_func = StringMap.find element_fname builtin_funcs in
      let _ =
        call_helper "_deep_free_array"
        [| param; free_element_func |] "" builder
      in
      let _ = L.build_ret_void builder in
      StringMap.add fname deep_free_func builtin_funcs
    in

    (* Build any necessary array-inserting functions *)
    let build_insert_array_func array_type builtin_funcs =
      let fname = "_insert_" ^ A.string_of_typ array_type in
      let element_t = ltype_of_typ (A.typ_of_container array_type) in
      let insert_t =
        L.function_type (pointer_t array_t)
        [| pointer_t array_t; i32_t; element_t |]
      in
      let insert_func = L.define_function fname insert_t the_module in
      let builder = L.builder_at_end context (L.entry_block insert_func) in

      (* Get params *)
      let params = L.params insert_func in
      (* Store pointer to element, cast to "void pointer" *)
      let element_ptr = L.build_alloca element_t "element_ptr" builder in
      let _ = L.build_store params.(2) element_ptr builder in
      let element_ptr = L.build_bitcast element_ptr (pointer_t i8_t) "element_ptr" builder in
      (* Replace element with its pointer *)
      let _ = Array.set params 2 element_ptr in

      (* Call insert_array *)
      let res = call_helper "_insert_array" params "res" builder in
      let _ = L.build_ret res builder in
      StringMap.add fname insert_func builtin_funcs
    in

    (* Build any necessary array-appending functions *)
    let build_append_array_func array_type builtin_funcs =
      let fname = "_append_" ^ A.string_of_typ array_type in
      let element_t = ltype_of_typ (A.typ_of_container array_type) in
      let append_t =
        L.function_type (pointer_t array_t)
        [| pointer_t array_t; element_t |]
      in
      let append_func = L.define_function fname append_t the_module in
      let builder = L.builder_at_end context (L.entry_block append_func) in

      (* Get params *)
      let params = L.params append_func in
      (* Store pointer to element, cast to "void pointer" *)
      let element_ptr = L.build_alloca element_t "element_ptr" builder in
      let _ = L.build_store params.(1) element_ptr builder in
      let element_ptr = L.build_bitcast element_ptr (pointer_t i8_t) "element_ptr" builder in
      (* Replace element with its pointer *)
      let _ = Array.set params 1 element_ptr in

      (* Call append_array *)
      let res = call_helper "_append_array" params "res" builder in
      let _ = L.build_ret res builder in
      StringMap.add fname append_func builtin_funcs
    in

    (* Build any necessary matrix reading functions *)
    let build_read_matrix_func matrix_type builtin_funcs =
      let fname =
        if matrix_type = A.Matrix A.Float then "_read_fmat_string" else "_read_imat_string"
      in
      let read_t = L.function_type (pointer_t matrix_t) [| pointer_t i8_t |] in
      let read_func = L.define_function fname read_t the_module in
      let builder = L.builder_at_end context (L.entry_block read_func) in

      (* Get params *)
      let filename = (L.params read_func).(0) in
      let mat_type = mat_type_of_typ (A.typ_of_container matrix_type) in

      (* Call _read_mat *)
      let res = call_helper "_read_mat" [| filename; mat_type |] "res" builder in
      let _ = L.build_ret res builder in
      StringMap.add fname read_func builtin_funcs
    in

    let fname_fragments = Str.split (Str.regexp "_") fname in
    match fname_fragments with
    | ["print"; arg_type] ->
        let arg_type = A.typ_of_string arg_type in
        (
          match arg_type with
          | A.Matrix _ ->
              build_external_builtin ~alias:(Some fname) "_print_matrix" builtin_funcs
          | A.Array _ -> build_print_array_func arg_type builtin_funcs
          | A.Func(_, _) -> build_print_function_func arg_type builtin_funcs
          | _ -> build_external_builtin fname builtin_funcs
        )
    | ["println"; arg_type] ->
        let arg_type = A.typ_of_string arg_type in
        build_println_func arg_type builtin_funcs
    | ["free"; arg_type] ->
        let arg_type = A.typ_of_string arg_type in
        (
          match arg_type with
          | A.Array _ ->
              build_external_builtin ~alias:(Some fname) "_free_array" builtin_funcs
          | A.Matrix _ ->
              build_external_builtin ~alias:(Some fname) "_free_matrix" builtin_funcs
          | _ -> make_err ("internal error: " ^ fname ^ " is invalid free")
        )
    | ["deep"; "free"; arg_type] ->
        let arg_type = A.typ_of_string arg_type in
        (
          match arg_type with
          | A.Array _ -> build_deep_free_func arg_type builtin_funcs
          | _ -> make_err ("internal error: " ^ fname ^ " is invalid deep_free")
        )
    | ["insert"; arg_type] ->
        let arg_type = A.typ_of_string arg_type in
        (
          match arg_type with
          | A.Matrix _ ->
              build_external_builtin ~alias:(Some fname) "_insert_matrix" builtin_funcs
          | A.Array _ -> build_insert_array_func arg_type builtin_funcs
          | _ -> make_err ("internal error: " ^ fname ^ " is invalid insert")
        )
    | ["delete"; arg_type] ->
        let arg_type = A.typ_of_string arg_type in
        (
          match arg_type with
          | A.Matrix _ ->
              build_external_builtin ~alias:(Some fname) "_delete_matrix" builtin_funcs
          | A.Array _ ->
              build_external_builtin ~alias:(Some fname) "_delete_array" builtin_funcs
          | _ -> make_err ("internal error: " ^ fname ^ " is invalid delete")
        )
    | ["append"; arg_type] ->
        let arg_type = A.typ_of_string arg_type in
        (
          match arg_type with
          | A.Matrix _ ->
              build_external_builtin ~alias:(Some fname) "_append_matrix" builtin_funcs
          | A.Array _ -> build_append_array_func arg_type builtin_funcs
          | _ -> make_err ("internal error: " ^ fname ^ " is invalid append")
        )
      | ["length"; _] | ["rows"; _] | ["cols"; _] | ["die"; _] ->
          build_external_builtin ~alias:(Some fname) ("_" ^ List.hd fname_fragments) builtin_funcs
      | ["to"; "int"; arg_type] | ["to"; "float"; arg_type] ->
          let arg_type = A.typ_of_string arg_type in
          (
            match arg_type with
            | A.Int ->
                build_external_builtin ~alias:(Some fname) "_int_to_float" builtin_funcs
            | A.Float ->
                build_external_builtin ~alias:(Some fname) "_float_to_int" builtin_funcs
            | A.Matrix _ ->
                build_external_builtin ~alias:(Some fname) "_flip_matrix_type" builtin_funcs
            | _ -> make_err ("internal error: " ^ fname ^ " is invalid cast")
          )
      | ["read"; "fmat"; _] -> build_read_matrix_func (A.Matrix A.Float) builtin_funcs
      | ["read"; "imat"; _] -> build_read_matrix_func (A.Matrix A.Int) builtin_funcs
      | ["write"; "mat"; _] -> build_external_builtin ~alias:(Some fname) "_write_mat" builtin_funcs
    | _ -> make_err ("internal error: unknown builtin function " ^ fname)
  in

  let builtin_funcs = StringSet.fold build_builtin builtin_uses StringMap.empty in

  (* Returns initial value for an empty declaration of a given type *)
  let init t =
    match t with
    | A.Float -> L.const_float float_t 0.0
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
    | A.Notyp ->
        make_err ("internal error: Notyp should have not made it " ^
        "past semant (init)")
  in

  (* Returns value of an identifier *)
  let rec lookup name scope =
    (* Redirect any attempt to call the program main through it's raw "main" name
     * to the renamed program main *)
    let name = if name = sys_main then prog_main else name in
    try StringMap.find name scope.variables with
    | Not_found ->
        match scope.parent with
        | None ->
            make_err ("internal error: semant should have rejected " ^
            "on undeclared identifier " ^ name)
        | Some parent -> lookup name parent
  in

  (* Builds a runtime check on a condition; if condition is not true,
   * then program prints error to stderr and exits with return code EXIT_FAILURE *)
  let build_check cond err builder =
    let _ = call_helper "_check" [| cond; err |] "" builder in
    ()
  in

  (* Builds a runtime check to see if a value is null *)
  let build_null_check value builder =
    let ltype = L.type_of value in
    let null = L.const_null ltype in
    (* We only use icmp here because the only null values in our language are for pointer
     * types (arrays, matrices, functions) *)
    let null_value_cond = L.build_icmp L.Icmp.Ne value null "null_value_cond" builder in
    build_check null_value_cond null_value_err builder
  in

  (* Builds a runtime check to see if binop values are valid *)
  let build_arith_check l_value op r_value builder =
    let ltype = L.type_of l_value in
    let not_equal =
      if ltype = float_t then L.build_fcmp L.Fcmp.One
      else L.build_icmp L.Icmp.Ne
    in
    let at_least =
      if ltype = float_t then L.build_fcmp L.Fcmp.Oge
      else L.build_icmp L.Icmp.Sge
    in
    let zero =
      if ltype = float_t then L.const_float float_t 0.
      else L.const_int i32_t 0
    in
    let is_int value builder =
      if ltype = float_t then
        let floor = call_helper "llvm.floor.f64" [| value |] "floor" builder in
        L.build_fcmp L.Fcmp.Oeq value floor "is_int" builder
      (* Ints are tautologically ints *)
      else L.const_int i1_t 1
    in
    match op with
    | A.Div | A.Mod ->
        let div_zero_cond = not_equal r_value zero "div_zero_cond" builder in
        build_check div_zero_cond div_zero_err builder
    | A.Exp ->
        let l_nonzero = not_equal l_value zero "l_nonzero" builder in
        let l_nonneg = at_least l_value zero "l_nonneg" builder in
        let r_nonneg = at_least r_value zero "r_nonneg" builder in
        let r_int = is_int r_value builder in
        let exp_zero_cond = L.build_or l_nonzero r_nonneg "exp_zero_cond" builder in
        let exp_neg_cond = L.build_or l_nonneg r_int "exp_neg_cond" builder in
        let _ = build_check exp_zero_cond exp_zero_err builder in
        build_check exp_neg_cond exp_neg_err builder
    | _ -> ()
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
      (* Check if the array body's elements are actually pointers;
        * see definition of array struct for explanation *)
      let array_has_ptrs typ =
        match typ with
        | A.Array _ | A.Matrix _ | A.Func(_, _) -> L.const_int i1_t 1
        | _ -> L.const_int i1_t 0
      in

      let build_array_lit typ raw_elements builder =
        let ltype = ltype_of_typ typ in
        let length = L.const_int i32_t (Array.length raw_elements) in
        let arr_ptr =
          call_helper "_malloc_array"
          [| length; L.size_of ltype; array_has_ptrs typ |] "arr_ptr" builder
        in

        (* Fill array body *)
        let set_element i element =
          let element_ptr = L.build_alloca ltype "element_ptr" builder in
          let _ = L.build_store element element_ptr builder in
          (* Cast element pointer to "void pointer" *)
          let element_ptr = L.build_bitcast element_ptr (pointer_t i8_t) "element_ptr" builder in
          let _ =
            call_helper "_set_array"
            [| arr_ptr; L.const_int i32_t i; element_ptr |] "" builder
          in
          ()
        in
        let _ = Array.iteri set_element raw_elements in
        arr_ptr
      in

      let build_matrix_lit typ raw_elements builder =
        let ltype = ltype_of_typ typ in
        let rows = L.const_int i32_t (Array.length raw_elements) in
        let cols = L.const_int i32_t (Array.length raw_elements.(0)) in
        let mat_type = mat_type_of_typ typ in
        let mat_ptr =
          call_helper "_malloc_matrix" [| rows; cols; mat_type |] "mat_ptr" builder
        in

        (* Allocate and fill matrix body *)
        let set_row i row =
          let set_element j element =
            let element_ptr = L.build_alloca ltype "element_ptr" builder in
            let _ = L.build_store element element_ptr builder in
            (* Cast element pointer to "void pointer" *)
            let element_ptr = L.build_bitcast element_ptr (pointer_t i8_t) "element_ptr" builder in
            let _ =
              call_helper "_set_matrix"
              [| mat_ptr; L.const_int i32_t i; L.const_int i32_t j; element_ptr |] "" builder
            in
            ()
          in
          Array.iteri set_element row
        in
        let _ = Array.iteri set_row raw_elements in
        mat_ptr
      in

      let build_sgl_slice arr slice builder =
        match slice with
        | SSlice(i, j) ->
            let slice_ptr = L.build_alloca slice_t "slice_ptr" builder in
            let slice_start = L.build_struct_gep slice_ptr 0 "slice_start" builder in
            let slice_end = L.build_struct_gep slice_ptr 1 "slice_end" builder in
            let i' = expr scope builder i in
            let j' =
              match snd j with
              | SInt_Lit _ -> expr scope builder j
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

      let build_dbl_slice mat row_slice col_slice builder =
        let get_rows mat builder =
          let rows_ptr = L.build_struct_gep mat 1 "rows_ptr" builder in
          L.build_load rows_ptr "rows" builder
        in

        let get_cols mat builder =
          let cols_ptr = L.build_struct_gep mat 2 "cols_ptr" builder in
          L.build_load cols_ptr "cols" builder
        in
        match (row_slice, col_slice) with
        | SSlice(i1, j1), SSlice(i2, j2) ->
            let row_slice_ptr = L.build_alloca slice_t "row_slice_ptr" builder in
            let row_slice_start = L.build_struct_gep row_slice_ptr 0 "row_slice_start" builder in
            let row_slice_end = L.build_struct_gep row_slice_ptr 1 "row_slice_end" builder in
            let col_slice_ptr = L.build_alloca slice_t "col_slice_ptr" builder in
            let col_slice_start = L.build_struct_gep col_slice_ptr 0 "col_slice_start" builder in
            let col_slice_end = L.build_struct_gep col_slice_ptr 1 "col_slice_end" builder in
            let i1' = expr scope builder i1 in
            let j1' =
              match snd j1 with
              | SInt_Lit _ -> expr scope builder j1
              | SSlice_Inc -> L.build_add i1' (L.const_int i32_t 1) "row_slice_inc" builder
              (* Otherwise, it's SEnd *)
              | _ -> get_rows mat builder
            in
            let i2' = expr scope builder i2 in
            let j2' =
              match snd j2 with
              | SInt_Lit _ -> expr scope builder j2
              | SSlice_Inc -> L.build_add i2' (L.const_int i32_t 1) "col_slice_inc" builder
              (* Otherwise, it's SEnd *)
              | _ -> get_cols mat builder
            in
            let _ = L.build_store i1' row_slice_start builder in
            let _ = L.build_store j1' row_slice_end builder in
            let _ = L.build_store i2' col_slice_start builder in
            let _ = L.build_store j2' col_slice_end builder in
            (row_slice_ptr, col_slice_ptr)
        | _ -> make_err "internal error: build_dbl_slice given non-slice"
      in

      let sexpr_of_sindex = function
        | SIndex e -> e
        | _ -> make_err "internal error: sexpr_of_sindex given non-index"
      in

      (* Used for element-wise matrix operations *)
      let get_mat_opcode op =
        match op with
        | A.Add -> 0
        | A.Sub -> 1
        | A.Mult -> 2
        | A.Div -> 3
        | A.Mod -> 4
        | A.Exp -> 5
        | A.Equal -> 6
        | A.Neq -> 7
        | A.Less -> 8
        | A.Leq -> 9
        | A.Greater -> 10
        | A.Geq -> 11
        | _ -> make_err (
            "internal error: op " ^ A.string_of_op op ^
            " is not an element-wise matrix op"
          )
      in

      (* TODO: perform runtime checks; division by zero, index out of bounds,
      * working with null arrays/matrices (i.e. global array/matrix that hasn't
      * been assigned a real value) *)
      match e with
      | SInt_Lit i -> L.const_int i32_t i
      | SBool_Lit b -> L.const_int i1_t (if b then 1 else 0)
      | SFloat_Lit l -> L.const_float_of_string float_t l
      | SString_Lit s -> L.build_global_stringptr (Scanf.unescaped s) "str" builder
      | SArray_Lit l ->
          let raw_array = Array.map (expr scope builder) l in
          let typ = A.typ_of_container t in
          build_array_lit typ raw_array builder
      | SEmpty_Array(t, n) ->
          let ltype = ltype_of_typ t in
          let length = expr scope builder n in
          let arr_ptr =
            call_helper "_malloc_array"
            [| length; L.size_of ltype; array_has_ptrs t |] "arr_ptr" builder
          in
          let init_ptr = L.build_alloca ltype "init_ptr" builder in
          let _ = L.build_store (init t) init_ptr builder in
          let init_ptr = L.build_bitcast init_ptr (pointer_t i8_t) "init_ptr" builder in
          let _ = call_helper "_init_array" [| arr_ptr; init_ptr |] "" builder in
          arr_ptr
      | SMatrix_Lit l ->
          let raw_elements = Array.map (Array.map (expr scope builder)) l in
          let typ = A.typ_of_container t in
          build_matrix_lit typ raw_elements builder
      | SEmpty_Matrix(t, r, c) ->
          let rows = expr scope builder r in
          let cols = expr scope builder c in
          let mat_ptr =
            call_helper "_malloc_matrix"
            [| rows; cols; mat_type_of_typ t |] "mat_ptr" builder
          in
          let _ = call_helper "_init_matrix" [| mat_ptr |] "" builder in
          mat_ptr
      | SIndex_Expr i ->
          (
            match i with
            | SSgl_Index(e, i) ->
                let ltype = ltype_of_typ t in
                let e' = expr scope builder e in
                let i' = expr scope builder (sexpr_of_sindex i) in
                let ptr = call_helper "_get_array" [| e'; i' |] "ptr" builder in
                let ptr = L.build_bitcast ptr (pointer_t ltype) "ptr" builder in
                L.build_load ptr "arr_element" builder
            | SDbl_Index(e, i, j) ->
                let ltype = ltype_of_typ t in
                let e' = expr scope builder e in
                let i' = expr scope builder (sexpr_of_sindex i) in
                let j' = expr scope builder (sexpr_of_sindex j) in
                let ptr =
                  call_helper "_get_matrix" [| e'; i'; j' |] "mat_element" builder
                in
                let ptr = L.build_bitcast ptr (pointer_t ltype) "ptr" builder in
                L.build_load ptr "mat_element" builder
          )
      | SSlice_Expr s ->
          (
            match s with
            | SSgl_Slice(e, s) ->
                let e' = expr scope builder e in
                let s' = build_sgl_slice e' s builder in
                call_helper "_slice_array" [| e'; s' |] "arr_slice" builder
            | SDbl_Slice(e, s1, s2) ->
                let e' = expr scope builder e in
                let s1', s2' = build_dbl_slice e' s1 s2 builder in
                call_helper "_slice_matrix" [| e'; s1'; s2' |] "mat_slice" builder
          )
      | SNoexpr -> init t
      | SId s ->
          let e' = L.build_load (lookup s scope) s builder in
          (* Only perform a null check if the type is a function pointer or container;
           * the rest are initialized to non-null values (see init) *)
          let _ =
            match t with
            | A.Array _ | A.Matrix _ | A.Func(_, _) -> build_null_check e' builder
            | _ -> ()
          in
          e'
      | SAssign(e1, e2) ->
          let t, e1 = e1 in
          let e2' = expr scope builder e2 in
          let _ =
            match e1 with
            | SId s ->
                let _ = L.build_store e2' (lookup s scope) builder in
                ()
            | SIndex_Expr i ->
                (
                  match i with
                  | SSgl_Index(e, i) ->
                      let ltype = ltype_of_typ t in
                      let e' = expr scope builder e in
                      let i' = expr scope builder (sexpr_of_sindex i) in
                      let ptr = L.build_alloca ltype "ptr" builder in
                      let _ = L.build_store e2' ptr builder in
                      let ptr = L.build_bitcast ptr (pointer_t i8_t) "ptr" builder in
                      let _ = call_helper "_set_array" [| e'; i'; ptr |] "" builder in
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
                        call_helper "_set_matrix"
                        [| e'; i'; j'; ptr |] "" builder
                      in
                      ()
                )
            | SSlice_Expr s ->
              (
                match s with
                | SSgl_Slice(e, s) ->
                    let e' = expr scope builder e in
                    let s' = build_sgl_slice e' s builder in
                    let _ =
                      call_helper "_set_slice_array"
                      [| e'; s'; e2' |] "" builder
                    in
                    ()
                | SDbl_Slice(e, s1, s2) ->
                    let e' = expr scope builder e in
                    let s1', s2' = build_dbl_slice e' s1 s2 builder in
                    let _ =
                      call_helper "_set_slice_matrix"
                      [| e'; s1'; s2'; e2' |] "" builder
                    in
                    ()
              )
            | _ -> make_err "internal error: semant should have rejected invalid assignment"
          in
          e2'
      | SBinop(e1, op, e2, is_update) ->
          let err =
            "internal error: semant should have rejected " ^
            A.string_of_op op ^ " on " ^ A.string_of_typ t
          in
          let t1 = fst e1 and t2 = fst e2 in
          let e1' = expr scope builder e1
          and e2' = expr scope builder e2 in
          if t1 = A.Float && t2 = A.Float then
          (
            let _ = build_arith_check e1' op e2' builder in
            match op with
            | A.Add -> L.build_fadd e1' e2' "temp" builder
            | A.Sub -> L.build_fsub e1' e2' "temp" builder
            | A.Mult -> L.build_fmul e1' e2' "temp" builder
            | A.Div -> L.build_fdiv e1' e2' "temp" builder
            | A.Mod -> L.build_frem e1' e2' "temp" builder
            | A.Exp -> call_helper "_fexp" [| e1'; e2'|] "temp" builder
            | A.Equal -> L.build_fcmp L.Fcmp.Oeq e1' e2' "temp" builder
            | A.Neq -> L.build_fcmp L.Fcmp.One e1' e2' "temp" builder
            | A.Less -> L.build_fcmp L.Fcmp.Olt e1' e2' "temp" builder
            | A.Leq -> L.build_fcmp L.Fcmp.Ole e1' e2' "temp" builder
            | A.Greater -> L.build_fcmp L.Fcmp.Ogt e1' e2' "temp" builder
            | A.Geq -> L.build_fcmp L.Fcmp.Oge e1' e2' "temp" builder
            | _ -> make_err err
          )
          else if t1 = A.Int && t2 = A.Int then
          (
            let _ = build_arith_check e1' op e2' builder in
            match op with
            | A.Add -> L.build_add e1' e2' "temp" builder
            | A.Sub -> L.build_sub e1' e2' "temp" builder
            | A.Mult -> L.build_mul e1' e2' "temp" builder
            | A.Div -> L.build_sdiv e1' e2' "temp" builder
            | A.Mod -> L.build_srem e1' e2' "temp" builder
            | A.Exp -> call_helper "_iexp" [| e1'; e2'|] "temp" builder
            | A.Equal -> L.build_icmp L.Icmp.Eq e1' e2' "temp" builder
            | A.Neq -> L.build_icmp L.Icmp.Ne e1' e2' "temp" builder
            | A.Less -> L.build_icmp L.Icmp.Slt e1' e2' "temp" builder
            | A.Leq -> L.build_icmp L.Icmp.Sle e1' e2' "temp" builder
            | A.Greater -> L.build_icmp L.Icmp.Sgt e1' e2' "temp" builder
            | A.Geq -> L.build_icmp L.Icmp.Sge e1' e2' "temp" builder
            | _ -> make_err err
          )
          else if t1 = A.Bool then
          (
            match op with
            | A.And -> L.build_and e1' e2' "temp" builder
            | A.Or -> L.build_or e1' e2' "temp" builder
            | _ -> make_err err
          )
          (* In these cases, we just do pointer comparison *)
          else if t1 = A.String || A.is_array t1 || A.is_func t1 then
            L.build_icmp L.Icmp.Eq e1' e2' "temp" builder
          (* Otherwise, it's a matrix result (either int or float) *)
          else
            (* If we're using expressions that return a new matrix and
             * are unreachable after computation, free those *)
            let free_e1 =
              (* If binop is actually an update operation, then our expression is of the form
               * mat = mat op e; we need to now free mat after computation *)
              is_update ||
              match snd e1 with
              | SMatrix_Lit _ | SSlice_Expr _ | SBinop(_, _, _, _) -> true
              | SAssign(_, _) -> O.is_unreachable_mat_assign (snd e1)
              | _ -> false
            in
            let free_e2 =
              match snd e2 with
              | SMatrix_Lit _ | SSlice_Expr _ | SBinop(_, _, _, _) -> true
              | SAssign(_, _) -> O.is_unreachable_mat_assign (snd e2)
              | _ -> false
            in
            let res =
              match op with
              | A.MatMult -> call_helper "_matmult" [| e1'; e2' |] "temp" builder
              | A.And | A.Or -> make_err err
              | _ ->
                  let opcode = L.const_int i32_t (get_mat_opcode op) in
                  call_helper "_mat_binop" [| e1'; opcode; e2' |] "temp"  builder
            in
            let _ =
              if free_e1 then
                let free_matrix_func = StringMap.find "_free_matrix" builtin_funcs in
                let _ = L.build_call free_matrix_func [| e1' |] "" builder in
                ()
            in
            let _ =
              if free_e2 then
                let free_matrix_func = StringMap.find "_free_matrix" builtin_funcs in
                let _ = L.build_call free_matrix_func [| e2' |] "" builder in
                ()
            in
            res
      | SUnop(op, e) ->
          let t, _ = e in
          let e' = expr scope builder e in
          let op' =
            match op with
            | A.Neg when t = A.Float -> L.build_fneg
            | A.Neg -> L.build_neg
            | A.Not -> L.build_not
          in
          op' e' "temp" builder
      | SCall(f, args) ->
          let f' = expr scope builder f in
          let args' = List.map (expr scope builder) args in
          let result =
            match t with
            | A.Void -> ""
            | _ -> "result"
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
      | Some _ -> ()
      | None -> ignore (instr builder)
    in

    (* Build the code for the given statement; return the builder for
      the statement's successor (i.e., the next instruction will be built
      after the one generated by this call) *)
    (* Imperative nature of statement processing entails imperative OCaml *)
    let rec stmt (scope, builder) = function
      | SBlock sl ->
          let parent_scope = scope in
          let scope = { variables = StringMap.empty; parent = Some parent_scope } in
          let _, builder = List.fold_left stmt (scope, builder) sl in
          (* Return to parent scope *)
          (parent_scope, builder)
        (* Generate code for this expression, return resulting builder *)
      | SExpr e ->
          let e' = expr scope builder e in
          let _ =
            match e with
            | A.Matrix _, (SAssign(_, _) as a) ->
                if O.is_unreachable_mat_assign a then
                let free_matrix_func = StringMap.find "_free_matrix" builtin_funcs in
                let _ = L.build_call free_matrix_func [| e' |] "" builder in
                ()
            | _ -> ()
          in
          (scope, builder)
      | SReturn e ->
          let _ =
            match fdecl.styp with
              (* Special "return nothing" instr *)
            | A.Void -> L.build_ret_void builder
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
    let builder =
      match builder with
      | None -> L.builder_at_end context (L.entry_block the_function)
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
    add_terminal builder (
      match fdecl.styp with
      | A.Void -> L.build_ret_void
      | _ -> L.build_ret (init fdecl.styp)
    )
  in

  let get_main_decl globals =
    let get_assign sdecl =
      let _, t, s, e' = sdecl in
      let s' = (t, SId s) in
      SExpr(t, SAssign(s', e'))
    in
    let has_assign sdecl =
      let _, _, _, e' = sdecl in
      snd e' <> SNoexpr
    in
    (* Assign global variables in main *)
    let globals = List.filter has_assign globals in
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
  (* Add pointers to internal functions to scope *)
  let global_scope =
    let add_internal_funcs fname f scope =
      let f_ptr = L.define_global (fname ^ "_ptr") f the_module in
      { scope with variables = StringMap.add fname f_ptr scope.variables }
    in
    StringMap.fold add_internal_funcs builtin_funcs global_scope
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
