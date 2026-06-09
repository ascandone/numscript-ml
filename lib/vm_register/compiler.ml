include Compiler_intf
open Syntax

let rec iter_result f = function
  | [] -> Ok ()
  | hd :: tl ->
    let ( let* ) = Result.bind in
    let* () = f hd in
    iter_result f tl
;;

type ctx =
  { instructions : Virtual_instruction.t Dynarray.t
  ; next_reg : int ref
  ; next_label_id : int ref
  ; typecheck_state : Typecheck_instruction.typecheck_state
  }

let get_fresh_dest ctx =
  let value = !(ctx.next_reg) in
  incr ctx.next_reg;
  value
;;

let get_next_label_id ctx ~prefix =
  let value = !(ctx.next_label_id) in
  incr ctx.next_label_id;
  match value with
  | 0 -> prefix
  | _ -> Format.sprintf "%s_%d" prefix value
;;

let push_instruction ctx instr =
  Typecheck_instruction.push_instruction ctx.typecheck_state instr;
  Dynarray.add_last ctx.instructions instr
;;

let push_instruction_dest ctx get_instr =
  let dest = get_fresh_dest ctx in
  push_instruction ctx (get_instr dest);
  dest
;;

let binop_of_infix (expr : Ast.binop) : Virtual_instruction.binary_op =
  match expr with
  | Ast.Add -> `add_int
  | Ast.Sub -> `sub_int
  | Ast.Div -> `mk_portion
;;

let rec compile_expr_to ~dest ctx = function
  | Ast.ExprVar _ -> failwith "[TODO] var"
  | Ast.ExprAccount str | Ast.ExprAsset str | Ast.ExprString str ->
    push_instruction ctx @@ Virtual_instruction.LoadConst { value = `String str; dest }
  | Ast.ExprInt n ->
    push_instruction ctx
    @@ Virtual_instruction.LoadConst { value = `Int (Int64.of_int n); dest }
  | Ast.ExprMonetaryLit (asset, amount) ->
    compile_infix ~dest ~op:`mk_monetary ctx asset amount
  | Ast.ExprInfix (op, left, right) ->
    let op =
      match op with
      | Ast.Add -> `add_int
      | Ast.Sub -> `sub_int
      | Ast.Div -> `mk_portion
    in
    compile_infix ~dest ~op ctx left right
  | Ast.ExprPerc _ -> failwith "[TODO] perc"
  | Ast.ExprFnCall _ -> failwith "[TODO] fn call"

and compile_infix ~dest ~(op : Virtual_instruction.binary_op) ctx left right =
  let left = compile_expr ctx left in
  let right = compile_expr ctx right in
  push_instruction ctx @@ Virtual_instruction.BinaryOp { op; left; right; dest }

and compile_expr ctx expr =
  let dest = get_fresh_dest ctx in
  compile_expr_to ctx ~dest expr;
  dest
;;

(** returns the register holding the total amount pulled  *)
let rec compile_source ~cap_reg ctx (source : Ast.source) =
  let ( let* ) = Result.bind in
  match source, cap_reg with
  | Ast.SrcAccountOverdraft { max_overdraft = None; _ }, None ->
    Error Common.UncappedOverdraft
  | Ast.SrcAllotment _, None -> Error Common.UncappedAllotment
  | Ast.SrcAccountOverdraft _, _ -> failwith "[TODO] overdraft"
  | Ast.SrcAccount name, cap ->
    let account = compile_expr ctx name in
    let dest = get_fresh_dest ctx in
    push_instruction ctx (Virtual_instruction.PullAccount { cap; account; dest });
    Ok dest
  | Ast.SrcInorder srcs, None ->
    let inorder_total_reg =
      push_instruction_dest ctx (fun dest ->
        Virtual_instruction.LoadConst { value = `Int 0L; dest })
    in
    let* () =
      iter_result
        (fun src ->
           let* pulled_reg = compile_source ctx ~cap_reg:None src in
           push_instruction
             ctx
             (Virtual_instruction.BinaryOp
                { op = `add_int
                ; dest = inorder_total_reg
                ; left = inorder_total_reg
                ; right = pulled_reg
                });
           Ok ())
        srcs
    in
    Ok inorder_total_reg
  | Ast.SrcInorder srcs, Some outer_cap_reg ->
    let inorder_total_reg =
      push_instruction_dest ctx (fun dest ->
        Virtual_instruction.LoadConst { value = `Int 0L; dest })
    in
    (* TODO collapse together nested inorders *)
    let end_label = get_next_label_id ctx ~prefix:"inorder_end" in
    let inorder_cap =
      push_instruction_dest ctx (fun dest ->
        Virtual_instruction.UnaryOp { op = `int_copy; arg = outer_cap_reg; dest })
    in
    let rec loop = function
      | [] -> Ok ()
      | src :: srcs ->
        let* inner_pulled_amt_reg = compile_source ~cap_reg:(Some inorder_cap) ctx src in
        (* inorder_total_reg += inner_pulled_amt_reg *)
        push_instruction
          ctx
          (Virtual_instruction.BinaryOp
             { op = `add_int
             ; dest = inorder_total_reg
             ; left = inorder_total_reg
             ; right = inner_pulled_amt_reg
             });
        (match srcs with
         | [] ->
           (* Last expression: no jumps needed *)
           Ok ()
         | _ ->
           (* inorder_cap -= inner_pulled_amt_reg *)
           push_instruction
             ctx
             (Virtual_instruction.BinaryOp
                { op = `sub_int
                ; dest = inorder_cap
                ; left = inorder_cap
                ; right = inner_pulled_amt_reg
                });
           push_instruction
             ctx
             (Virtual_instruction.JmpIfZero { value = inorder_cap; label = end_label });
           loop srcs)
    in
    let* () = loop srcs in
    push_instruction ctx (Virtual_instruction.Label end_label);
    Ok inorder_total_reg
  | Ast.SrcMax (clause_cap, sub_src), _ ->
    let clause_cap_monetary_reg = compile_expr ctx clause_cap in
    let clause_cap_int_reg =
      push_instruction_dest ctx (fun dest ->
        Virtual_instruction.UnaryOp
          { op = `get_amount; arg = clause_cap_monetary_reg; dest })
    in
    let cap_reg =
      match cap_reg with
      | None -> clause_cap_int_reg
      | Some outer_cap_reg ->
        push_instruction_dest ctx (fun dest ->
          Virtual_instruction.BinaryOp
            { op = `min_int; left = clause_cap_int_reg; right = outer_cap_reg; dest })
    in
    compile_source ~cap_reg:(Some cap_reg) ctx sub_src
  | Ast.SrcAllotment _, Some _ -> failwith "[TODO] impl allot"

and compile_source_with_required_amt ~cap_reg ctx src =
  let ( let* ) = Result.bind in
  let* got = compile_source ~cap_reg:(Some cap_reg) ctx src in
  push_instruction ctx (Virtual_instruction.CheckEnoughFunds { got; needed = cap_reg });
  Ok got
;;

let rec compile_dest ~pulled_amt_reg ~current_cap ctx =
  let ( let* ) = Result.bind in
  function
  | Ast.DestAccount account_expr ->
    let account = compile_expr ctx account_expr in
    let cap =
      if Int.equal pulled_amt_reg current_cap then None else Some pulled_amt_reg
    in
    push_instruction ctx (Virtual_instruction.SendToAccount { account; cap });
    Ok ()
  | Ast.DestInorder (clauses, remaining) ->
    let* () =
      iter_result
        (fun ({ cap; dest } : Ast.dest_inorder_clause) ->
           let cap_monetary_reg = compile_expr ctx cap in
           let inner_pulled_amt_reg =
             push_instruction_dest ctx (fun dest ->
               Virtual_instruction.UnaryOp
                 { op = `get_amount; arg = cap_monetary_reg; dest })
           in
           let pulled_amt_reg =
             push_instruction_dest ctx (fun dest ->
               Virtual_instruction.BinaryOp
                 { op = `min_int
                 ; left = pulled_amt_reg
                 ; right = inner_pulled_amt_reg
                 ; dest
                 })
           in
           compile_kept_or_dest ctx ~pulled_amt_reg ~current_cap dest)
        clauses
    in
    compile_kept_or_dest ~pulled_amt_reg ~current_cap ctx remaining
  | Ast.DestAllotment _ -> failwith "[TODO] impl allotment dest"

and compile_kept_or_dest ~pulled_amt_reg ~current_cap ctx = function
  | Ast.Dest account_expr -> compile_dest ~pulled_amt_reg ~current_cap ctx account_expr
  | Ast.Kept -> failwith "[TODO] compile kept"
;;

let compile_stmt ctx =
  let ( let* ) = Result.bind in
  function
  | Ast.StmtSend { monetary; source; destination } ->
    let monetary_reg = compile_expr ctx monetary in
    let asset_reg =
      push_instruction_dest ctx (fun dest ->
        Virtual_instruction.UnaryOp { op = `get_asset; arg = monetary_reg; dest })
    in
    push_instruction ctx (Virtual_instruction.SetCurrentAsset { asset = asset_reg });
    let cap_reg =
      push_instruction_dest ctx (fun dest ->
        Virtual_instruction.UnaryOp { op = `get_amount; arg = monetary_reg; dest })
    in
    let* pulled_amt_reg = compile_source_with_required_amt ~cap_reg ctx source in
    let* () = compile_dest ~pulled_amt_reg ~current_cap:pulled_amt_reg ctx destination in
    Ok ()
  | Ast.StmtSendAll { asset; source; destination } ->
    let asset_reg = compile_expr ctx asset in
    push_instruction ctx (Virtual_instruction.SetCurrentAsset { asset = asset_reg });
    let* pulled_amt_reg = compile_source ~cap_reg:None ctx source in
    let* () = compile_dest ~pulled_amt_reg ~current_cap:pulled_amt_reg ctx destination in
    Ok ()
  | Ast.Save _ -> failwith "[TODO] compile stmt"
  | Ast.FnStatement _ -> failwith "[TODO] compile stmt"
;;

let compile_parsed (program : Ast.program) =
  let ( let* ) = Result.bind in
  let ctx : ctx =
    { instructions = Dynarray.create ()
    ; next_reg = ref 0
    ; next_label_id = ref 0
    ; typecheck_state = Typecheck_instruction.create_state ()
    }
  in
  let* () = iter_result (compile_stmt ctx) program.statements in
  let compiled : compiled_program =
    { instructions = Dynarray.to_array ctx.instructions }
  in
  Ok compiled
;;

let test_compiled ?(optimize = false) source =
  let parsed_ast = Syntax.Parser.parse source in
  let result =
    try compile_parsed parsed_ast with
    | Typecheck_instruction.TypecheckErr err ->
      failwith (Typecheck_instruction.show_typecheck_err err)
  in
  let compiled_program =
    match result with
    | Error e -> failwith (Common.show_compilation_err e)
    | Ok compiled -> compiled
  in
  let instructions =
    if optimize
    then Peephole_all.run compiled_program.instructions
    else compiled_program.instructions
  in
  Format.printf "%a" Virtual_instruction.pp_program instructions
;;

let%expect_test "empty program" =
  test_compiled {||};
  [%expect {| |}]
;;

let%expect_test "simple program" =
  test_compiled
    {|
    send [USD/2 10] (
      source = @src
      destination = @dest
    )
  |};
  [%expect
    {|
    $r1 <- load_const("USD/2")
    $r2 <- load_const(10)
    $r0 <- mk_monetary($r1, $r2)
    $r3 <- get_asset($r0)
    set_current_asset($r3)
    $r4 <- get_amount($r0)
    $r5 <- load_const("src")
    $r6 <- pull_account($r5, $r4)
    check_enough_funds($r6, $r4)
    $r7 <- load_const("dest")
    send_to_account_uncapped($r7)
    |}]
;;

let%expect_test "simple program (optimized)" =
  test_compiled
    ~optimize:true
    {|
    send [USD/2 10] (
      source = @src
      destination = @dest
    )
  |};
  [%expect
    {|
    $r3 <- load_const("USD/2")
    set_current_asset($r3)
    $r4 <- load_const(10)
    $r5 <- load_const("src")
    $r6 <- pull_account($r5, $r4)
    check_enough_funds($r6, $r4)
    $r7 <- load_const("dest")
    send_to_account_uncapped($r7)
    |}]
;;

let%expect_test "inorder" =
  test_compiled
    {|
    send [USD/2 10] (
      source = {
        @s1
        @s2
      }
      destination = @dest
    )
  |};
  [%expect
    {|
    $r1 <- load_const("USD/2")
    $r2 <- load_const(10)
    $r0 <- mk_monetary($r1, $r2)
    $r3 <- get_asset($r0)
    set_current_asset($r3)
    $r4 <- get_amount($r0)
    $r5 <- load_const(0)
    $r6 <- int_copy($r4)
    $r7 <- load_const("s1")
    $r8 <- pull_account($r7, $r6)
    $r5 <- add_int($r5, $r8)
    $r6 <- sub_int($r6, $r8)
    jmp_if_zero($r6, #inorder_end)
    $r9 <- load_const("s2")
    $r10 <- pull_account($r9, $r6)
    $r5 <- add_int($r5, $r10)
    #inorder_end
    check_enough_funds($r5, $r4)
    $r11 <- load_const("dest")
    send_to_account_uncapped($r11)
    |}]
;;

let%expect_test "top level max" =
  test_compiled
    {|
    send [USD/2 10] (
      source = max [USD/2 5] from @s1
      destination = @dest
    )
  |};
  [%expect
    {|
    $r1 <- load_const("USD/2")
    $r2 <- load_const(10)
    $r0 <- mk_monetary($r1, $r2)
    $r3 <- get_asset($r0)
    set_current_asset($r3)
    $r4 <- get_amount($r0)
    $r6 <- load_const("USD/2")
    $r7 <- load_const(5)
    $r5 <- mk_monetary($r6, $r7)
    $r8 <- get_amount($r5)
    $r9 <- min_int($r8, $r4)
    $r10 <- load_const("s1")
    $r11 <- pull_account($r10, $r9)
    check_enough_funds($r11, $r4)
    $r12 <- load_const("dest")
    send_to_account_uncapped($r12)
    |}]
;;

let%expect_test "top level max on unbounded" =
  test_compiled
    {|
    send [USD/2 *] (
      source = max [USD/2 5] from @s1
      destination = @dest
    )
  |};
  [%expect
    {|
    $r0 <- load_const("USD/2")
    set_current_asset($r0)
    $r2 <- load_const("USD/2")
    $r3 <- load_const(5)
    $r1 <- mk_monetary($r2, $r3)
    $r4 <- get_amount($r1)
    $r5 <- load_const("s1")
    $r6 <- pull_account($r5, $r4)
    $r7 <- load_const("dest")
    send_to_account_uncapped($r7)
    |}]
;;

let%expect_test "capped + inorder" =
  test_compiled
    {|
    send [USD/2 10] (
      source = {
        max [USD/2 5] from @s1
        @s2
      }
      destination = @dest
    )
  |};
  [%expect
    {|
    $r1 <- load_const("USD/2")
    $r2 <- load_const(10)
    $r0 <- mk_monetary($r1, $r2)
    $r3 <- get_asset($r0)
    set_current_asset($r3)
    $r4 <- get_amount($r0)
    $r5 <- load_const(0)
    $r6 <- int_copy($r4)
    $r8 <- load_const("USD/2")
    $r9 <- load_const(5)
    $r7 <- mk_monetary($r8, $r9)
    $r10 <- get_amount($r7)
    $r11 <- min_int($r10, $r6)
    $r12 <- load_const("s1")
    $r13 <- pull_account($r12, $r11)
    $r5 <- add_int($r5, $r13)
    $r6 <- sub_int($r6, $r13)
    jmp_if_zero($r6, #inorder_end)
    $r14 <- load_const("s2")
    $r15 <- pull_account($r14, $r6)
    $r5 <- add_int($r5, $r15)
    #inorder_end
    check_enough_funds($r5, $r4)
    $r16 <- load_const("dest")
    send_to_account_uncapped($r16)
    |}]
;;

let%expect_test "capped + inorder (optimized)" =
  test_compiled
    ~optimize:true
    {|
    send [USD/2 10] (
      source = {
        max [USD/2 5] from @s1
        @s2
      }
      destination = @dest
    )
  |};
  [%expect
    {|
    $r3 <- load_const("USD/2")
    set_current_asset($r3)
    $r4 <- load_const(10)
    $r5 <- load_const(0)
    $r6 <- load_const(10)
    $r11 <- load_const(5)
    $r12 <- load_const("s1")
    $r13 <- pull_account($r12, $r11)
    $r5 <- add_int($r5, $r13)
    $r6 <- sub_int($r6, $r13)
    jmp_if_zero($r6, #inorder_end)
    $r14 <- load_const("s2")
    $r15 <- pull_account($r14, $r6)
    $r5 <- add_int($r5, $r15)
    #inorder_end
    check_enough_funds($r5, $r4)
    $r16 <- load_const("dest")
    send_to_account_uncapped($r16)
    |}]
;;

let%expect_test "inorder dest remaining" =
  test_compiled
    {|
    send [USD/2 10] (
      source = @src
      destination = {
        remaining to @dest
      }
    )
  |};
  [%expect
    {|
    $r1 <- load_const("USD/2")
    $r2 <- load_const(10)
    $r0 <- mk_monetary($r1, $r2)
    $r3 <- get_asset($r0)
    set_current_asset($r3)
    $r4 <- get_amount($r0)
    $r5 <- load_const("src")
    $r6 <- pull_account($r5, $r4)
    check_enough_funds($r6, $r4)
    $r7 <- load_const("dest")
    send_to_account_uncapped($r7)
    |}]
;;

let%expect_test "inorder dest clauses" =
  test_compiled
    {|
    send [USD/2 10] (
      source = @src
      destination = {
        max [USD/2 5] to @dest:capped
        remaining to @dest
      }
    )
  |};
  [%expect
    {|
    $r1 <- load_const("USD/2")
    $r2 <- load_const(10)
    $r0 <- mk_monetary($r1, $r2)
    $r3 <- get_asset($r0)
    set_current_asset($r3)
    $r4 <- get_amount($r0)
    $r5 <- load_const("src")
    $r6 <- pull_account($r5, $r4)
    check_enough_funds($r6, $r4)
    $r8 <- load_const("USD/2")
    $r9 <- load_const(5)
    $r7 <- mk_monetary($r8, $r9)
    $r10 <- get_amount($r7)
    $r11 <- min_int($r6, $r10)
    $r12 <- load_const("dest:capped")
    send_to_account_capped($r12, $r11)
    $r13 <- load_const("dest")
    send_to_account_uncapped($r13)
    |}]
;;

let%expect_test "uncapped src" =
  test_compiled
    {|
    send [USD/2 *] (
      source = @src
      destination = @dest
    )
  |};
  [%expect
    {|
    $r0 <- load_const("USD/2")
    set_current_asset($r0)
    $r1 <- load_const("src")
    $r2 <- pull_account_uncapped($r1)
    $r3 <- load_const("dest")
    send_to_account_uncapped($r3)
    |}]
;;

let%expect_test "uncapped inorder" =
  test_compiled
    {|
    send [USD/2 *] (
      source = {
        @src1
        @src2
      }
      destination = @dest
    )
  |};
  [%expect
    {|
    $r0 <- load_const("USD/2")
    set_current_asset($r0)
    $r1 <- load_const(0)
    $r2 <- load_const("src1")
    $r3 <- pull_account_uncapped($r2)
    $r1 <- add_int($r1, $r3)
    $r4 <- load_const("src2")
    $r5 <- pull_account_uncapped($r4)
    $r1 <- add_int($r1, $r5)
    $r6 <- load_const("dest")
    send_to_account_uncapped($r6)
    |}]
;;
