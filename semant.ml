(* Semantic checking for the Neo compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Check if a certain kind of binding has void type or is a duplicate
     of another, previously checked binding *)
  (*
  let check_binds (kind : string) (to_check : bind list) =
    let check_it checked binding =
      let void_err = "illegal void " ^ kind ^ " " ^ snd binding
      and dup_err = "duplicate " ^ kind ^ " " ^ snd binding in
      match binding with
          (* No void bindings *)
          (Void, _) -> raise (Failure void_err)
        | (_, n1) -> match checked with
            (* No duplicate bindings *)
              ((_, n2) :: _) when n1 = n2 -> raise (Failure dup_err)
            | _ -> binding :: checked
    in
    let _ = List.fold_left check_it [] (List.sort compare to_check) in
    to_check
  in
  *)

  (* TODO: check expression and keyword *)
  (*
  let check_decls (to_check: decl list) =
    let check_it checked (_, t, s, _) = (* TODO: check kw and e *)
      let void_err = "illegal void " ^ s
      and dup_err = "duplicate " ^ s in
      match (t, s) with
          (* No void bindings *)
          (Void, _) -> raise (Failure void_err)
        | (_, n1) -> match checked with
            (* No duplicate bindings *)
              ((_, n2) :: _) when n1 = n2 -> raise (Failure dup_err)
            | _ -> (t, s) :: checked
    in
    let _ = List.fold_left check_it [] (List.sort compare to_check) in
    to_check
  in
  *)

  (**** Checking Functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    let add_bind map name = StringMap.add name {
      typ = Void; fname = name;
      formals = [];
      body = [] } map
    in
    List.fold_left add_bind StringMap.empty [
      "print";
      "printf";
    ]
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in
    match fd with (* No duplicate functions or redefinitions of built-ins *)
        _ when StringMap.mem n built_in_decls -> make_err built_in_err
      | _ when StringMap.mem n map -> make_err dup_err
      | _ ->  StringMap.add n fd map
  in

  (* Collect all other function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  (* Return a variable from our local symbol table *)
  let type_of_identifier symbols s =
    try StringMap.find s symbols
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in

  (* Return a semantically-checked expression, i.e., with a type *)
  let rec check_expr symbols expr = match expr with
      Int_Lit  l -> (Int, SInt_Lit l)
    | Float_Lit l -> (Float, SFloat_Lit l)
    | Bool_Lit l  -> (Bool, SBool_Lit l)
    | Noexpr     -> (Void, SNoexpr)
    | Id s       -> (type_of_identifier symbols s, SId s)
    | Matrix_Lit _ as m -> check_container_lit symbols m
    | Assign(e1, _, e2) -> (* TODO: check e1 for index/string, and op *)
        (
          match e1 with
              Id s ->
                let lt = type_of_identifier symbols s
                and (rt, e') = check_expr symbols e2 in
                let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt ^ " in " ^ string_of_expr expr
                in
                (check_assign lt rt err, SAssign(check_expr symbols e1, Noop, (rt, e')))
            | _ -> raise (Failure "not supported yet in assign")
        )
    | Unop(op, e) ->
        let (t, e') = check_expr symbols e in
        let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^
                                string_of_uop op ^ string_of_typ t ^
                                " in " ^ string_of_expr expr))
        in
        (ty, SUnop(op, (t, e')))
    | Binop(e1, op, e2) ->
        let (t1, e1') = check_expr symbols e1
        and (t2, e2') = check_expr symbols e2 in
        (* All binary operators require operands of the same type *)
        let same = t1 = t2 in
        (* Determine expression type based on operator and operand types *)
        let ty = match op with
            Add | Sub | Mult | Div     when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div     when same && t1 = Float -> Float
          | Equal | Neq                when same               -> Bool
          | Less | Leq | Greater | Geq when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or                   when same && t1 = Bool -> Bool
          | _ -> raise (
              Failure ("illegal binary operator " ^
                            string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                            string_of_typ t2 ^ " in " ^ string_of_expr expr))
        in
        (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call("print", args) ->
          if List.length args != 1 then raise (Failure "expecting 1 argument in print")
          else
            let arg = List.hd (List.map (check_expr symbols) args) in
            let ty = fst arg in
            (
              match ty with
                  Int | Bool | Matrix _ -> (ty, SCall("print", [arg]))
                | _ -> raise (Failure ("type " ^ string_of_typ ty ^ " not supported by print"))
            )
      | Call(fname, args) as call ->
          let fd = find_func fname in
          let param_length = List.length fd.formals in
            if List.length args != param_length then
              raise (Failure ("expecting " ^ string_of_int param_length ^
                              " arguments in " ^ string_of_expr call))
            else let check_call (_, ft, _, _) e =
              let (et, e') = check_expr symbols e in
              let err = "illegal argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
              in
              (check_assign ft et err, e')
        in
        let args' = List.map2 check_call fd.formals args in
        (fd.typ, SCall(fname, args'))
      | _ -> raise (Failure "not supported yet in check_expr")

  (* check all elements in container literal have valid type and have valid sizes *)
  and check_container_lit symbols e =
    let check_size e = match e with
        Matrix_Lit l ->
          if Array.length(l) > 0 && Array.length(l.(0)) > 0 then e
          else raise (Failure (string_of_matrix l ^ " must have non-zero dimensions"))
      | _ -> raise (Failure (string_of_expr e ^ " is not a supported container type"))
    in

    let check_equal_type t e =
      let (t', e') = check_expr symbols e in
      if t == t' then (t', e')
      else raise (Failure ("container expected type " ^ string_of_typ t ^
        " but saw type " ^ string_of_typ t'))
    in

    let _ = check_size e in
    match e with
        Matrix_Lit l ->
          let t = fst (check_expr symbols l.(0).(0)) in
          (Matrix(t), SMatrix_Lit(Array.map (Array.map (check_equal_type t)) l))
      | _ -> raise (Failure "internal error: check_size should have rejected")

  (* TODO: check for void/duplicate locals in each block *)
  (* Raise an exception if the given rvalue type cannot be assigned to
      the given lvalue type *)
  and check_assign lvaluet rvaluet err =
    if lvaluet = rvaluet then lvaluet else raise (Failure err)
  in

  let check_decl symbols (kw, t, s, expr) =
    let void_err = "illegal void " ^ s in
    let sexpr = check_expr symbols expr in
    let et = fst sexpr in
    let typ_err = "declared type " ^ string_of_typ t ^
       " but initialized with type " ^ string_of_typ et
    in (* TODO: check for dup *)
    if t != et then raise (Failure typ_err)
    else
      if t == Void then raise (Failure void_err)
      else (kw, t, s, sexpr)
  in

  (**** Checking Global Variables ****)

  let globals' = List.map (check_decl StringMap.empty) globals in

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    (* TODO: check for dup *)
    let formals' = List.map (check_decl StringMap.empty) func.formals in

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (_, ty, name, _) -> StringMap.add name ty m)
	    StringMap.empty (globals' @ formals')
    in

    let check_bool_expr e =
      let (t', e') = check_expr symbols e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in
      if t' != Bool then raise (Failure err) else (t', e')
    in

    let check_for_initializer = function
        I_Expr e -> SI_Expr (check_expr symbols e)
      | I_Decl d -> SI_Decl (check_decl symbols d)
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (check_expr symbols e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(i, e1, e2, st) ->
	        SFor(check_for_initializer i, check_bool_expr e1, check_expr symbols e2, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = check_expr symbols e in
          if t = func.typ then SReturn (t, e')
          else raise (
            Failure ("return gives " ^ string_of_typ t ^ " expected " ^
              string_of_typ func.typ ^ " in " ^ string_of_expr e))

	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl ->
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in
          SBlock(check_stmt_list sl)
      | _ -> raise (Failure "not supported yet in check_stmt")

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = formals';
      sbody = match check_stmt (Block func.body) with
          SBlock(sl) -> sl
        | _ -> let err = "internal error: block didn't become a block?" in
            raise (Failure err)
    }
  in
  (globals', List.map check_function functions)
