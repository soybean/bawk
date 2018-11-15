type action = Ast | (*Sast |*) LLVM_IR | Compile

let () =
  let action = ref Ast in
  let set_action a () = action := a in
  let input_file = ref "" in
  let set_input input = input_file := input in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    (*("-s", Arg.Unit (set_action Sast), "Print the SAST");*)
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
    ("-f", Arg.String (set_input), "Specifies the input to read");
  ] in  
  let usage_msg = "usage: ./bawk.native [-a|-s|-l|-c] [file.bawk] [-f] [input]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
(*match !action with
    Ast -> print_string (Ast.string_of_program ast)
  | _ -> let sast = Semant.check ast in*)
    match !action with
    (*Ast     -> ()*)
      Ast -> print_string (Ast.string_of_program ast)
  (*| Sast    -> print_string (Sast.string_of_sprogram sast)*)
    | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate ast input_file))
    | Compile -> let m = Codegen.translate ast input_file in
  Llvm_analysis.assert_valid_module m;
  print_string (Llvm.string_of_llmodule m)
