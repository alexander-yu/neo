let () =
  let usage_msg = "usage: ./toplevel.native [file.neo]" in
  let channel = ref stdin in
  Arg.parse [] (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  print_string (Ast.string_of_program ast)
