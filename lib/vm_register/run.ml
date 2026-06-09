let run_program ~vars ~balances (program : Syntax.Ast.program) =
  let ( let* ) = Result.bind in
  let* compiled =
    Compiler.compile_parsed program |> Result.map_error (fun e -> `Compilation e)
  in
  let vm = Abstract_vm.create ~instructions:compiled.instructions in
  let* postings =
    Abstract_vm.run vm ~vars ~balances |> Result.map_error (fun e -> `Runtime e)
  in
  Ok postings
;;
