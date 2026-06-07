include Compiler_intf
open Syntax

type var_resolution =
  | VarResolution_External
  | VarResolution_Internal of int

type ctx =
  { string_like_constants : string Stack.t
  ; vars : (string, var_resolution * Common.expr_typ) Hashtbl.t
  ; int_constants : int64 Stack.t
  ; array_constants : int array Stack.t
  ; expr_bytecode : Program.op_expr Stack.t
  ; expr_bytecode_chunks : Program.expr_chunk Stack.t
  ; sources : Program.op_source Stack.t
  ; source_patches : (int, Program.op_source) Hashtbl.t
  ; destinations : Program.op_dest Stack.t
  ; destination_patches : (int, Program.op_dest) Hashtbl.t
  ; statements : Program.op_stmt Stack.t
  ; next_var_uid : int ref
  }

let rec map_result f = function
  | [] -> Ok []
  | hd :: tl ->
    let ( let* ) = Result.bind in
    let* hd = f hd in
    let* tl = map_result f tl in
    Ok (hd :: tl)
;;

let rec iter_result f = function
  | [] -> Ok ()
  | hd :: tl ->
    let ( let* ) = Result.bind in
    let* () = f hd in
    iter_result f tl
;;

let push_stack_idx value stack =
  let index = Stack.length stack in
  Stack.push value stack;
  index
;;

let compile_int_const ctx num =
  let num =
    (* TODO should the syntax have int64 or int nums? *)
    Int64.of_int num
  in
  (* TODO(perf) reuse constants *)
  let pool_idx = push_stack_idx num ctx.int_constants in
  Stack.push (Program.Expr_FetchConst { pool = `Int; pool_idx }) ctx.expr_bytecode
;;

let rec compile_expr ctx =
  let ( let* ) = Result.bind in
  function
  | Ast.ExprVar name ->
    let* resolution, typ =
      Hashtbl.find_opt ctx.vars name |> Option.to_result ~none:(UnboundVar name)
    in
    (match resolution with
     | VarResolution_External ->
       let name_idx = push_stack_idx name ctx.string_like_constants in
       Stack.push (Program.Expr_FetchVar { typ; name_idx }) ctx.expr_bytecode;
       Ok ()
     | VarResolution_Internal uid ->
       Stack.push (Program.Expr_GetLocal { typ; uid }) ctx.expr_bytecode;
       Ok ())
  | Ast.ExprAccount name | Ast.ExprString name | Ast.ExprAsset name ->
    (* TODO(perf) reuse constants *)
    let pool_idx = push_stack_idx name ctx.string_like_constants in
    Stack.push
      (Program.Expr_FetchConst { pool = `StringLike; pool_idx })
      ctx.expr_bytecode;
    Ok ()
  | Ast.ExprPerc num ->
    (* TODO we need to take care of the comma as well *)
    compile_int_const ctx num;
    compile_int_const ctx 100;
    Stack.push Program.Expr_MkPortion ctx.expr_bytecode;
    Ok ()
  | Ast.ExprInt num ->
    compile_int_const ctx num;
    Ok ()
  | Ast.ExprInfix (op, l, r) ->
    let* () = compile_expr ctx r in
    let* () = compile_expr ctx l in
    Stack.push
      (match op with
       | Ast.Add -> Program.Expr_NumAdd
       | Ast.Sub -> Program.Expr_NumSub
       | Ast.Div -> Program.Expr_MkPortion)
      ctx.expr_bytecode;
    Ok ()
  | Ast.ExprMonetaryLit (asset, amount) ->
    let* () = compile_expr ctx asset in
    let* () = compile_expr ctx amount in
    Stack.push Program.Expr_MkMonetary ctx.expr_bytecode;
    Ok ()
  | Ast.ExprFnCall (("meta" as fn_name), args) ->
    (match args with
     | [ _; _ ] -> failwith "TODO fn call meta"
     | _ -> Error (BadArity { fn_name; received = List.length args }))
  | Ast.ExprFnCall (("balance" as fn_name), args) ->
    (match args with
     | [ _; _ ] -> failwith "TODO fn balance"
     | _ -> Error (BadArity { fn_name; received = List.length args }))
  | Ast.ExprFnCall (fn_name, _) -> Error (InvalidFn { fn_name })
;;

let compile_expr_chunk ctx expr =
  let ( let* ) = Result.bind in
  (* TODO(bug) this may not be accurate: we shouldn't count instructions,
    we should count bytes maybe?  *)
  let start_idx = Stack.length ctx.expr_bytecode in
  let* () = compile_expr ctx expr in
  let end_idx = Stack.length ctx.expr_bytecode in
  let chunk : Program.expr_chunk = { start_idx; size = end_idx - start_idx } in
  let chunk_id = push_stack_idx chunk ctx.expr_bytecode_chunks in
  Ok chunk_id
;;

let rec compile_source ctx =
  let ( let* ) = Result.bind in
  function
  | Ast.SrcAccount acc_name_expr ->
    let* account_expr_idx = compile_expr_chunk ctx acc_name_expr in
    Stack.push (Program.Src_Account { account_expr_idx }) ctx.sources;
    Ok ()
  | Ast.SrcAccountOverdraft { account; max_overdraft = None } ->
    let* account_expr_idx = compile_expr_chunk ctx account in
    Stack.push (Program.Src_AccountUnbounded { account_expr_idx }) ctx.sources;
    Ok ()
  | Ast.SrcAccountOverdraft { account; max_overdraft = Some overdraft_expr } ->
    let* account_expr_idx = compile_expr_chunk ctx account in
    let* overdraft_expr_idx = compile_expr_chunk ctx overdraft_expr in
    Stack.push
      (Program.Src_AccountBoundedOverdraft { account_expr_idx; overdraft_expr_idx })
      ctx.sources;
    Ok ()
  | Ast.SrcMax (cap, sub_source) ->
    let* monetary_expr_idx = compile_expr_chunk ctx cap in
    Stack.push (Program.Src_Max { monetary_expr_idx }) ctx.sources;
    let* () = compile_source ctx sub_source in
    Ok ()
  | Ast.SrcInorder sources ->
    (* We push a dummy instruction first, as we'll only know the end index after
      compilation of all the sources *)
    let inorder_src_idx =
      push_stack_idx (Program.Src_Inorder { end_idx = -1 }) ctx.sources
    in
    let* () = iter_result (compile_source ctx) sources in
    (* TODO check: do we want end_index or end_index + 1 ? *)
    let end_idx = Stack.length ctx.sources in
    Hashtbl.replace ctx.source_patches inorder_src_idx (Program.Src_Inorder { end_idx });
    Ok ()
  | Ast.SrcAllotment clauses ->
    (* TODO: if at least one portion is variable, rem is mandatory *)
    (* TODO: sum(known portions) must be <= 1 *)
    let por_clauses =
      List.filter_map (fun (expr, src) -> Option.map (fun expr -> expr, src) expr) clauses
    in
    let rem_clause =
      List.find_opt (fun (expr, _) -> Option.is_none expr) clauses |> Option.map snd
    in
    let* por_array =
      por_clauses
      |> map_result (fun (expr, _) -> compile_expr_chunk ctx expr)
      |> Result.map Array.of_list
    in
    let array_idx = push_stack_idx por_array ctx.array_constants in
    Stack.push (Program.Src_Allotment { array_const_idx = array_idx }) ctx.sources;
    let* () = iter_result (fun (_, source) -> compile_source ctx source) por_clauses in
    let* () =
      match rem_clause with
      | None -> Ok ()
      | Some rem_src -> compile_source ctx rem_src
    in
    Ok ()
;;

let rec compile_destination ctx =
  let ( let* ) = Result.bind in
  function
  | Ast.DestAccount acc_name_expr ->
    let* account_expr_idx = compile_expr_chunk ctx acc_name_expr in
    Stack.push (Program.Dest_Account { account_expr_idx }) ctx.destinations;
    Ok ()
  | Ast.DestInorder (clauses, rem) ->
    (* TODO do we need to iter on reversed list? *)
    let* () =
      iter_result
        (fun (clause : Ast.dest_inorder_clause) ->
           let* monetary_expr_idx = compile_expr_chunk ctx clause.cap in
           Stack.push (Program.Dest_Max { monetary_expr_idx }) ctx.destinations;
           compile_destination_or_kept ctx clause.dest)
        clauses
    in
    compile_destination_or_kept ctx rem
  | Ast.DestAllotment _ -> failwith "[TODO] dest allotment"

and compile_destination_or_kept ctx = function
  | Ast.Dest sub_dest -> compile_destination ctx sub_dest
  | Ast.Kept ->
    Stack.push Program.Dest_Kept ctx.destinations;
    Ok ()
;;

let compile_stmt ctx =
  let source_idx = Stack.length ctx.sources in
  let destination_idx = Stack.length ctx.destinations in
  let ( let* ) = Result.bind in
  function
  | Ast.StmtSendAll { asset; source; destination } ->
    let* asset_expr_idx = compile_expr_chunk ctx asset in
    let* () = compile_source ctx source in
    let* () = compile_destination ctx destination in
    Ok (Program.Stmt_SendAll { asset_expr_idx; source_idx; destination_idx })
  | Ast.StmtSend { monetary; source; destination } ->
    let* monetary_expr_idx = compile_expr_chunk ctx monetary in
    let* () = compile_source ctx source in
    let* () = compile_destination ctx destination in
    Ok (Program.Stmt_Send { monetary_expr_idx; source_idx; destination_idx })
  | Ast.Save { monetary; account } ->
    let* monetary_expr_idx = compile_expr_chunk ctx monetary in
    let* account_expr_idx = compile_expr_chunk ctx account in
    Ok (Program.Stmt_Save { monetary_expr_idx; account_expr_idx })
  | Ast.FnStatement { name = "set_tx_meta" as fn_name; args } ->
    (match args with
     | [ _k; _v ] -> failwith "TODO compile fn"
     | _ -> Error (BadArity { fn_name; received = List.length args }))
  | Ast.FnStatement { name = "set_account_meta" as fn_name; args } ->
    (match args with
     | [ _acc; _k; _v ] -> failwith "TODO compile fn"
     | _ -> Error (BadArity { fn_name; received = List.length args }))
  | Ast.FnStatement { name; _ } -> Error (InvalidFn { fn_name = name })
;;

let stack_to_array stack =
  stack |> Stack.to_seq |> List.of_seq |> List.rev |> Array.of_list
;;

let stack_to_array_patched ~patches stack =
  (* TODO check we didn't mess up the patches index when reversing *)
  stack
  |> Stack.to_seq
  |> List.of_seq
  |> List.rev
  |> List.mapi (fun index op ->
    match Hashtbl.find_opt patches index with
    | None -> op
    | Some patch -> patch)
  |> Array.of_list
;;

let compile_var ctx (var : Ast.var) =
  let ( let* ) = Result.bind in
  let* typ = Common.parse_typ var.typ |> Option.to_result ~none:(InvalidType var.typ) in
  let* binding =
    match var.value with
    | None ->
      (* TODO(perf) instead of always inlining it, we should count the var occurrences,
          and if it's >1, we should pre-fetch it once, and save it as "internal" *)
      Ok VarResolution_External
    | Some expr ->
      (* TODO(perf) we can inline the var, AS LONG AS it's used exactly once AND it's pure (no balance() calls)
            (NOTE: make sure tricky edge cases are handled, like referencing impure vars)
         *)
      let var_uid = !(ctx.next_var_uid) in
      incr ctx.next_var_uid;
      let* expr_idx = compile_expr_chunk ctx expr in
      Stack.push (Program.Stmt_SetLocal { var_uid; typ; expr_idx }) ctx.statements;
      Ok (VarResolution_Internal var_uid)
  in
  Hashtbl.replace ctx.vars var.name (binding, typ);
  Ok ()
;;

let compile_parsed (program_ast : Syntax.Ast.program) =
  let ( let* ) = Result.bind in
  let ctx : ctx =
    { vars = Hashtbl.create 5
    ; string_like_constants = Stack.create ()
    ; int_constants = Stack.create ()
    ; array_constants = Stack.create ()
    ; expr_bytecode = Stack.create ()
    ; expr_bytecode_chunks = Stack.create ()
    ; sources = Stack.create ()
    ; source_patches = Hashtbl.create 4
    ; destinations = Stack.create ()
    ; destination_patches = Hashtbl.create 4
    ; statements = Stack.create ()
    ; next_var_uid = ref 0
    }
  in
  let* () = program_ast.vars |> iter_result (compile_var ctx) in
  let* () =
    program_ast.statements
    |> iter_result (fun stm ->
      let* out = compile_stmt ctx stm in
      Stack.push out ctx.statements;
      Ok ())
  in
  let constant_pool : Program.constant_pool =
    { string_like = stack_to_array ctx.string_like_constants
    ; int = stack_to_array ctx.int_constants
    ; array = stack_to_array ctx.array_constants
    }
  in
  Ok
    { Program.constant_pool
    ; statements = stack_to_array ctx.statements
    ; sources = stack_to_array_patched ~patches:ctx.source_patches ctx.sources
    ; destinations =
        stack_to_array_patched ~patches:ctx.destination_patches ctx.destinations
    ; expr_bytecode = stack_to_array ctx.expr_bytecode
    ; expr_chunks = stack_to_array ctx.expr_bytecode_chunks
    }
;;

(* -- Test *)

let test_compiled source =
  let parsed_ast = Syntax.Parser.parse source in
  let result = compile_parsed parsed_ast in
  let compiled_program =
    match result with
    | Error e -> failwith (show_compilation_err e)
    | Ok compiled -> compiled
  in
  print_endline (Program.show compiled_program)
;;

let%expect_test "empty program" =
  test_compiled {||};
  [%expect
    {|
    { constant_pool = { string_like = [||]; int = [||]; array = [||] };
      statements = [||]; sources = [||]; destinations = [||];
      expr_bytecode = [||]; expr_chunks = [||] }
    |}]
;;

let%expect_test "simple send" =
  test_compiled
    {|
    send [USD/2 10] (
      source = @src
      destination = @dest
    )

  |};
  [%expect
    {|
    { constant_pool =
      { string_like = [|"USD/2"; "src"; "dest"|]; int = [|10L|]; array = [||] };
      statements =
      [|Stmt_Send {monetary_expr_idx = 0; source_idx = 0; destination_idx = 0}|];
      sources = [|Src_Account {account_expr_idx = 1}|];
      destinations = [|Dest_Account {account_expr_idx = 2}|];
      expr_bytecode =
      [|Expr_FetchConst {pool = `StringLike; pool_idx = 0};
        Expr_FetchConst {pool = `Int; pool_idx = 0}; Expr_MkMonetary;
        Expr_FetchConst {pool = `StringLike; pool_idx = 1};
        Expr_FetchConst {pool = `StringLike; pool_idx = 2}|];
      expr_chunks =
      [|{ start_idx = 0; size = 3 }; { start_idx = 3; size = 1 };
        { start_idx = 4; size = 1 }|]
      }
    |}]
;;

let%expect_test "inorder" =
  test_compiled
    {|
    send [USD/2 10] (
      source = {
        @src1
        @src2
      }
      destination = @dest
    )

  |};
  [%expect
    {|
    { constant_pool =
      { string_like = [|"USD/2"; "src1"; "src2"; "dest"|]; int = [|10L|];
        array = [||] };
      statements =
      [|Stmt_Send {monetary_expr_idx = 0; source_idx = 0; destination_idx = 0}|];
      sources =
      [|Src_Inorder {end_idx = 3}; Src_Account {account_expr_idx = 1};
        Src_Account {account_expr_idx = 2}|];
      destinations = [|Dest_Account {account_expr_idx = 3}|];
      expr_bytecode =
      [|Expr_FetchConst {pool = `StringLike; pool_idx = 0};
        Expr_FetchConst {pool = `Int; pool_idx = 0}; Expr_MkMonetary;
        Expr_FetchConst {pool = `StringLike; pool_idx = 1};
        Expr_FetchConst {pool = `StringLike; pool_idx = 2};
        Expr_FetchConst {pool = `StringLike; pool_idx = 3}|];
      expr_chunks =
      [|{ start_idx = 0; size = 3 }; { start_idx = 3; size = 1 };
        { start_idx = 4; size = 1 }; { start_idx = 5; size = 1 }|]
      }
    |}]
;;

let%expect_test "inorder + nested max" =
  test_compiled
    {|
    send [USD/2 10] (
      source = {
        max [USD/2 5] from @src1
        @src2
      }
      destination = @dest
    )

  |};
  [%expect
    {|
    { constant_pool =
      { string_like = [|"USD/2"; "USD/2"; "src1"; "src2"; "dest"|];
        int = [|10L; 5L|]; array = [||] };
      statements =
      [|Stmt_Send {monetary_expr_idx = 0; source_idx = 0; destination_idx = 0}|];
      sources =
      [|Src_Inorder {end_idx = 4}; Src_Max {monetary_expr_idx = 1};
        Src_Account {account_expr_idx = 2}; Src_Account {account_expr_idx = 3}|];
      destinations = [|Dest_Account {account_expr_idx = 4}|];
      expr_bytecode =
      [|Expr_FetchConst {pool = `StringLike; pool_idx = 0};
        Expr_FetchConst {pool = `Int; pool_idx = 0}; Expr_MkMonetary;
        Expr_FetchConst {pool = `StringLike; pool_idx = 1};
        Expr_FetchConst {pool = `Int; pool_idx = 1}; Expr_MkMonetary;
        Expr_FetchConst {pool = `StringLike; pool_idx = 2};
        Expr_FetchConst {pool = `StringLike; pool_idx = 3};
        Expr_FetchConst {pool = `StringLike; pool_idx = 4}|];
      expr_chunks =
      [|{ start_idx = 0; size = 3 }; { start_idx = 3; size = 3 };
        { start_idx = 6; size = 1 }; { start_idx = 7; size = 1 };
        { start_idx = 8; size = 1 }|]
      }
    |}]
;;

let%expect_test "extern vars" =
  test_compiled
    {|
    vars {
      monetary $m
      account $src
    }

    send $m (
      source = $src
      destination = @dest
    )

  |};
  [%expect
    {|
    { constant_pool =
      { string_like = [|"m"; "src"; "dest"|]; int = [||]; array = [||] };
      statements =
      [|Stmt_Send {monetary_expr_idx = 0; source_idx = 0; destination_idx = 0}|];
      sources = [|Src_Account {account_expr_idx = 1}|];
      destinations = [|Dest_Account {account_expr_idx = 2}|];
      expr_bytecode =
      [|Expr_FetchVar {typ = ExprTyp_Monetary; name_idx = 0};
        Expr_FetchVar {typ = ExprTyp_Account; name_idx = 1};
        Expr_FetchConst {pool = `StringLike; pool_idx = 2}|];
      expr_chunks =
      [|{ start_idx = 0; size = 1 }; { start_idx = 1; size = 1 };
        { start_idx = 2; size = 1 }|]
      }
    |}]
;;

let%expect_test "internal vars" =
  test_compiled
    {|
    vars {
      account $src_acc = @acc
      account $dest_acc = $src_acc
    }

    send [USD/2 *] (
      source = $src_acc
      destination = $dest_acc
    )

  |};
  [%expect
    {|
    { constant_pool =
      { string_like = [|"acc"; "USD/2"|]; int = [||]; array = [||] };
      statements =
      [|Stmt_SetLocal {var_uid = 0; typ = ExprTyp_Account; expr_idx = 0};
        Stmt_SetLocal {var_uid = 1; typ = ExprTyp_Account; expr_idx = 1};
        Stmt_SendAll {asset_expr_idx = 2; source_idx = 0; destination_idx = 0}|];
      sources = [|Src_Account {account_expr_idx = 3}|];
      destinations = [|Dest_Account {account_expr_idx = 4}|];
      expr_bytecode =
      [|Expr_FetchConst {pool = `StringLike; pool_idx = 0};
        Expr_GetLocal {typ = ExprTyp_Account; uid = 0};
        Expr_FetchConst {pool = `StringLike; pool_idx = 1};
        Expr_GetLocal {typ = ExprTyp_Account; uid = 0};
        Expr_GetLocal {typ = ExprTyp_Account; uid = 1}|];
      expr_chunks =
      [|{ start_idx = 0; size = 1 }; { start_idx = 1; size = 1 };
        { start_idx = 2; size = 1 }; { start_idx = 3; size = 1 };
        { start_idx = 4; size = 1 }|]
      }
    |}]
;;

let%expect_test "internal vars" =
  test_compiled
    {|
    send [USD/2 *] (
      source = @acc
      destination = {
        max [USD/2 100] to @a
        max [USD/2 200] to @b
        remaining to @c
      }
    )

  |};
  [%expect
    {|
    { constant_pool =
      { string_like = [|"USD/2"; "acc"; "USD/2"; "a"; "USD/2"; "b"; "c"|];
        int = [|100L; 200L|]; array = [||] };
      statements =
      [|Stmt_SendAll {asset_expr_idx = 0; source_idx = 0; destination_idx = 0}|];
      sources = [|Src_Account {account_expr_idx = 1}|];
      destinations =
      [|Dest_Max {monetary_expr_idx = 2}; Dest_Account {account_expr_idx = 3};
        Dest_Max {monetary_expr_idx = 4}; Dest_Account {account_expr_idx = 5};
        Dest_Account {account_expr_idx = 6}|];
      expr_bytecode =
      [|Expr_FetchConst {pool = `StringLike; pool_idx = 0};
        Expr_FetchConst {pool = `StringLike; pool_idx = 1};
        Expr_FetchConst {pool = `StringLike; pool_idx = 2};
        Expr_FetchConst {pool = `Int; pool_idx = 0}; Expr_MkMonetary;
        Expr_FetchConst {pool = `StringLike; pool_idx = 3};
        Expr_FetchConst {pool = `StringLike; pool_idx = 4};
        Expr_FetchConst {pool = `Int; pool_idx = 1}; Expr_MkMonetary;
        Expr_FetchConst {pool = `StringLike; pool_idx = 5};
        Expr_FetchConst {pool = `StringLike; pool_idx = 6}|];
      expr_chunks =
      [|{ start_idx = 0; size = 1 }; { start_idx = 1; size = 1 };
        { start_idx = 2; size = 3 }; { start_idx = 5; size = 1 };
        { start_idx = 6; size = 3 }; { start_idx = 9; size = 1 };
        { start_idx = 10; size = 1 }|]
      }
    |}]
;;

let%expect_test "allotment" =
  test_compiled
    {|
    send [USD/2 *] (
      source = {
        1/3 from @s1
        1/4 from @s2
        remaining from @s3
      }
      destination = @dest
    )
  |};
  [%expect
    {|
    { constant_pool =
      { string_like = [|"USD/2"; "s1"; "s2"; "s3"; "dest"|];
        int = [|3L; 1L; 4L; 1L|]; array = [|[|1; 2|]|] };
      statements =
      [|Stmt_SendAll {asset_expr_idx = 0; source_idx = 0; destination_idx = 0}|];
      sources =
      [|Src_Allotment {array_const_idx = 0}; Src_Account {account_expr_idx = 3};
        Src_Account {account_expr_idx = 4}; Src_Account {account_expr_idx = 5}|];
      destinations = [|Dest_Account {account_expr_idx = 6}|];
      expr_bytecode =
      [|Expr_FetchConst {pool = `StringLike; pool_idx = 0};
        Expr_FetchConst {pool = `Int; pool_idx = 0};
        Expr_FetchConst {pool = `Int; pool_idx = 1}; Expr_MkPortion;
        Expr_FetchConst {pool = `Int; pool_idx = 2};
        Expr_FetchConst {pool = `Int; pool_idx = 3}; Expr_MkPortion;
        Expr_FetchConst {pool = `StringLike; pool_idx = 1};
        Expr_FetchConst {pool = `StringLike; pool_idx = 2};
        Expr_FetchConst {pool = `StringLike; pool_idx = 3};
        Expr_FetchConst {pool = `StringLike; pool_idx = 4}|];
      expr_chunks =
      [|{ start_idx = 0; size = 1 }; { start_idx = 1; size = 3 };
        { start_idx = 4; size = 3 }; { start_idx = 7; size = 1 };
        { start_idx = 8; size = 1 }; { start_idx = 9; size = 1 };
        { start_idx = 10; size = 1 }|]
      }
    |}]
;;
