let program_testable = Numscript.Vm.Program.(Alcotest.testable pp equal)

let test_compiled ~source ~expected =
  let open Numscript in
  let parsed_ast = Syntax.Parser.parse source in
  let result = Vm.Compiler.compile_parsed parsed_ast in
  let compiled_program =
    match result with
    | Error e -> Alcotest.fail (Vm.Compiler.show_compilation_err e)
    | Ok compiled -> compiled
  in
  Alcotest.check
    program_testable
    "expected the program to be compiled as"
    expected
    compiled_program
;;

let empty_program : Numscript.Vm.Program.t =
  { constant_pool = { string_like = [||]; int = [||] }
  ; statements = [||]
  ; sources = [||]
  ; destinations = [||]
  ; expr_chunks = [||]
  ; expr_bytecode = [||]
  }
;;

let vm_tests =
  let open Numscript.Vm.Program in
  [ ("empty", `Quick, fun () -> test_compiled ~source:{||} ~expected:empty_program)
  ; ( "simple send"
    , `Quick
    , fun () ->
        test_compiled
          ~source:
            {|
    send [USD/2 10] (
      source = @src
      destination = @dest
    )
  |}
          ~expected:
            { constant_pool =
                { string_like = [| "USD/2"; "src"; "dest" |]; int = [| 10L |] }
            ; statements =
                [| Stmt_Send
                     { monetary_expr_idx = 0; source_idx = 0; destination_idx = 0 }
                |]
            ; sources = [| Src_Account { account_expr_idx = 1 } |]
            ; destinations = [| Dest_Account { account_expr_idx = 2 } |]
            ; expr_bytecode =
                [| Expr_FetchConst { pool = `StringLike; pool_idx = 0 }
                 ; Expr_FetchConst { pool = `Int; pool_idx = 0 }
                 ; Expr_MkMonetary
                 ; Expr_FetchConst { pool = `StringLike; pool_idx = 1 }
                 ; Expr_FetchConst { pool = `StringLike; pool_idx = 2 }
                |]
            ; expr_chunks =
                [| { start_idx = 0; size = 3 }
                 ; { start_idx = 3; size = 1 }
                 ; { start_idx = 4; size = 1 }
                |]
            } )
  ; ( "inorder"
    , `Quick
    , fun () ->
        test_compiled
          ~source:
            {|
    send [USD/2 10] (
      source = {
        @src1
        @src2
      }
      destination = @dest
    )
  |}
          ~expected:
            { constant_pool =
                { string_like = [| "USD/2"; "src1"; "src2"; "dest" |]; int = [| 10L |] }
            ; statements =
                [| Stmt_Send
                     { monetary_expr_idx = 0; source_idx = 0; destination_idx = 0 }
                |]
            ; sources =
                [| Src_Inorder { end_idx = 3 }
                 ; Src_Account { account_expr_idx = 1 }
                 ; Src_Account { account_expr_idx = 2 }
                |]
            ; destinations = [| Dest_Account { account_expr_idx = 3 } |]
            ; expr_bytecode =
                [| Expr_FetchConst { pool = `StringLike; pool_idx = 0 } (* e0  *)
                 ; Expr_FetchConst { pool = `Int; pool_idx = 0 }
                 ; Expr_MkMonetary
                 ; Expr_FetchConst { pool = `StringLike; pool_idx = 1 } (* e1 *)
                 ; Expr_FetchConst { pool = `StringLike; pool_idx = 2 } (* e2 *)
                 ; Expr_FetchConst { pool = `StringLike; pool_idx = 3 } (* e3 *)
                |]
            ; expr_chunks =
                [| { start_idx = 0; size = 3 }
                 ; { start_idx = 3; size = 1 }
                 ; { start_idx = 4; size = 1 }
                 ; { start_idx = 5; size = 1 }
                |]
            } )
  ; ( "inorder + nested max"
    , `Quick
    , fun () ->
        test_compiled
          ~source:
            {|
    send [USD/2 10] (
      source = {
        max [USD/2 5] from @src1
        @src2
      }
      destination = @dest
    )
  |}
          ~expected:
            { constant_pool =
                { string_like = [| "USD/2"; "USD/2"; "src1"; "src2"; "dest" |]
                ; int = [| 10L; 5L |]
                }
            ; statements =
                [| Stmt_Send
                     { monetary_expr_idx = 0; source_idx = 0; destination_idx = 0 }
                |]
            ; sources =
                [| Src_Inorder { end_idx = 4 }
                 ; Src_Max { monetary_expr_idx = 1 }
                 ; Src_Account { account_expr_idx = 2 }
                 ; Src_Account { account_expr_idx = 3 }
                |]
            ; destinations = [| Dest_Account { account_expr_idx = 4 } |]
            ; expr_bytecode =
                [| Expr_FetchConst { pool = `StringLike; pool_idx = 0 }
                 ; Expr_FetchConst { pool = `Int; pool_idx = 0 }
                 ; Expr_MkMonetary
                 ; Expr_FetchConst { pool = `StringLike; pool_idx = 1 }
                 ; Expr_FetchConst { pool = `Int; pool_idx = 1 }
                 ; Expr_MkMonetary
                 ; Expr_FetchConst { pool = `StringLike; pool_idx = 2 }
                 ; Expr_FetchConst { pool = `StringLike; pool_idx = 3 }
                 ; Expr_FetchConst { pool = `StringLike; pool_idx = 4 }
                |]
            ; expr_chunks =
                [| { start_idx = 0; size = 3 }
                 ; { start_idx = 3; size = 3 }
                 ; { start_idx = 6; size = 1 }
                 ; { start_idx = 7; size = 1 }
                 ; { start_idx = 8; size = 1 }
                |]
            } )
  ]
;;

let () = Alcotest.run "Test Suite" [ "VM Compilation", vm_tests ]
