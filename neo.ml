(* Author(s): Alex *)
(* Top-level of the Neo compiler: scan & parse the input,
 * check the resulting AST and generate an SAST from it, generate LLVM IR,
 * and dump the module *)

type action = Ast | Sast | Optimize | LLVM_IR | Compile

let make_err err = raise (Failure err)

let scan_error lexbuf ch =
  let start_p = Lexing.lexeme_start_p lexbuf in
  let end_p = Lexing.lexeme_end_p lexbuf in
  let line = string_of_int start_p.Lexing.pos_lnum in
  let ch_start = string_of_int (start_p.Lexing.pos_cnum - start_p.Lexing.pos_bol) in
  let ch_end = string_of_int (end_p.Lexing.pos_cnum - end_p.Lexing.pos_bol) in
  make_err ("Illegal character at line " ^ line ^ ", characters " ^ ch_start
   ^ "-" ^ ch_end ^ ": " ^ "\"" ^ ch ^ "\"")

let parse_error lexbuf =
  let start_p = Lexing.lexeme_start_p lexbuf in
  let end_p = Lexing.lexeme_end_p lexbuf in
  let line = string_of_int start_p.Lexing.pos_lnum in
  let ch_start = string_of_int (start_p.Lexing.pos_cnum - start_p.Lexing.pos_bol) in
  let ch_end = string_of_int (end_p.Lexing.pos_cnum - end_p.Lexing.pos_bol) in
  let lexeme = Lexing.lexeme lexbuf in
  make_err ("Syntax error at line " ^ line ^ ", characters " ^ ch_start
  ^ "-" ^ ch_end ^ ": " ^ lexeme)

let parse lexbuf =
  try Parser.program Scanner.token lexbuf with
  | Scanner.Scan_error ch -> scan_error lexbuf ch
  | Parsing.Parse_error -> parse_error lexbuf

let () =
  let action = ref Compile in
  let channel = ref stdin in
  let set_action a () = action := a in
  let set_channel channel filename =
    let file =
      try open_in filename with
      | Sys_error s ->
          let _ = print_endline s in
          exit 1
    in
    channel := file
  in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-o", Arg.Unit (set_action Optimize), "Print the optimized SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in
  let usage_msg = "usage: ./neo.native [-a|-s|-l|-c] [file.neo]" in
  (* Parse args *)
  let _ = Arg.parse speclist (set_channel channel) usage_msg in

  (* Get buffers/channels for file and stdlib *)
  let std_channel = ref stdin in
  let _ = set_channel std_channel "stdlib.neo" in
  let lexbuf = Lexing.from_channel !channel
  and std_lexbuf = Lexing.from_channel !std_channel in

  (* Parse program and standard library *)
  let ast = parse lexbuf in
  let std_ast = parse std_lexbuf in

  (* Prepend standard library to program *)
  let ast = (fst ast, snd std_ast @ snd ast) in

  match !action with
  | Ast -> print_string (Ast.string_of_program ast)
  | _ ->
      let sast = Semant.check ast in
      match !action with
      | Ast -> ()
      | Sast -> print_string (Sast.string_of_sprogram sast)
      | Optimize ->
          let sast = Optimize.prune_uses sast in
          print_string (Sast.string_of_sprogram (snd sast))
      | LLVM_IR ->
          let sast = Optimize.prune_uses sast in
          let m = Codegen.translate sast in
          print_string (Llvm.string_of_llmodule m)
      | Compile ->
          let sast = Optimize.prune_uses sast in
          let m = Codegen.translate sast in
          let _ = Llvm_analysis.assert_valid_module m in
          print_string (Llvm.string_of_llmodule m)
