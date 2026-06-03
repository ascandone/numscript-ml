include Compiler_intf
open Syntax

type ctx =
  { string_like_constants : string Stack.t
  ; int_constants : int64 Stack.t
  ; expr_bytecode : Program.op_expr Stack.t
  ; expr_bytecode_chunks : Program.expr_chunk Stack.t
  ; sources : Program.op_source Stack.t
  ; source_patches : (int, Program.op_source) Hashtbl.t
  ; destinations : Program.op_dest Stack.t
  ; destination_patches : (int, Program.op_dest) Hashtbl.t
  ; statements : Program.op_stmt Stack.t
  }

let rec iter_map f = function
  | [] -> Ok []
  | hd :: tl ->
    let ( let* ) = Result.bind in
    let* hd = f hd in
    let* tl = iter_map f tl in
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
  | Ast.ExprVar _ -> failwith "[TODO] impl vars"
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
    let* () = compile_expr ctx l in
    let* () = compile_expr ctx r in
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
    let* account_idx = compile_expr_chunk ctx acc_name_expr in
    Stack.push (Program.Src_Account { account_idx }) ctx.sources;
    Ok ()
  | Ast.SrcMax (cap, sub_source) ->
    let* monetary_idx = compile_expr_chunk ctx cap in
    Stack.push (Program.Src_Max { monetary_idx }) ctx.sources;
    let* () = compile_source ctx sub_source in
    Ok ()
  | Ast.SrcInorder sources ->
    let* () = iter_result (compile_source ctx) sources in
    (* We push a dummy instruction first, as we'll only know the end index after
      compilation of all the sources *)
    let inorder_src_idx =
      push_stack_idx (Program.Src_Inorder { end_idx = -1 }) ctx.sources
    in
    (* TODO check: do we want end_index or end_index + 1 ? *)
    let end_idx = Stack.length ctx.sources in
    Hashtbl.replace ctx.source_patches inorder_src_idx (Program.Src_Inorder { end_idx });
    Ok ()
  | Ast.SrcAccountOverdraft _ -> failwith "TODO overdraft"
  | Ast.SrcAllotment _ -> failwith "TODO allotmnent"
;;

let rec compile_destination ctx =
  let ( let* ) = Result.bind in
  function
  | Ast.DestAccount acc_name_expr ->
    let* account_idx = compile_expr_chunk ctx acc_name_expr in
    Stack.push (Program.Dest_Account { account_idx }) ctx.destinations;
    Ok ()
  | _ -> failwith "TODO dest"
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

let compile_parsed (program_ast : Syntax.Ast.program) =
  let ( let* ) = Result.bind in
  let ctx : ctx =
    { string_like_constants = Stack.create ()
    ; int_constants = Stack.create ()
    ; expr_bytecode = Stack.create ()
    ; expr_bytecode_chunks = Stack.create ()
    ; sources = Stack.create ()
    ; source_patches = Hashtbl.create 4
    ; destinations = Stack.create ()
    ; destination_patches = Hashtbl.create 4
    ; statements = Stack.create ()
    }
  in
  let* stmts_list = program_ast.statements |> iter_map (compile_stmt ctx) in
  let constant_pool : Program.constant_pool =
    { string_like = stack_to_array ctx.string_like_constants
    ; int = stack_to_array ctx.int_constants
    }
  in
  Ok
    { Program.constant_pool
    ; statements = Array.of_list stmts_list
    ; sources = stack_to_array_patched ~patches:ctx.source_patches ctx.sources
    ; destinations =
        stack_to_array_patched ~patches:ctx.destination_patches ctx.destinations
    ; expr_bytecode = stack_to_array ctx.expr_bytecode
    ; expr_chunks = stack_to_array ctx.expr_bytecode_chunks
    }
;;
