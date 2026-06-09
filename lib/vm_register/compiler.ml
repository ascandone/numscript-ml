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
  incr ctx.next_reg;
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

let rec compile_source ~pulled_amt_reg ~cap_reg ctx (source : Ast.source) =
  let ( let* ) = Result.bind in
  match source, cap_reg with
  | Ast.SrcAccountOverdraft { max_overdraft = None; _ }, None ->
    Error Common.UncappedOverdraft
  | Ast.SrcAccountOverdraft _, _ -> failwith "[TODO] overdraft"
  | Ast.SrcAccount name, None ->
    let _account_reg = compile_expr ctx name in
    failwith "[TODO] uncapped pull"
  | Ast.SrcAccount name, Some cap ->
    let account = compile_expr ctx name in
    push_instruction
      ctx
      (Virtual_instruction.PullAccount { cap; account; dest = pulled_amt_reg });
    Ok ()
  | Ast.SrcInorder _, None -> Error UncappedInorder
  | Ast.SrcInorder srcs, Some outer_cap_reg ->
    (* TODO collapse together nested inorders *)
    let end_label = get_next_label_id ctx ~prefix:"inorder_end" in
    let inorder_cap =
      push_instruction_dest ctx (fun dest ->
        Virtual_instruction.UnaryOp { op = `int_copy; arg = outer_cap_reg; dest })
    in
    let rec loop = function
      | [] -> Ok ()
      | src :: srcs ->
        let inner_pulled_amt_reg = get_fresh_dest ctx in
        let* () =
          compile_source
            ~pulled_amt_reg:inner_pulled_amt_reg
            ~cap_reg:(Some inorder_cap)
            ctx
            src
        in
        push_instruction
          ctx
          (Virtual_instruction.BinaryOp
             { op = `add_int
             ; dest = pulled_amt_reg
             ; left = pulled_amt_reg
             ; right = inner_pulled_amt_reg
             });
        (match srcs with
         | [] ->
           (* Last expression: no jumps needed *)
           Ok ()
         | _ ->
           (* inorder_cap -= pulled_amt *)
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
    Ok ()
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
    compile_source ~pulled_amt_reg ~cap_reg:(Some cap_reg) ctx sub_src
  | Ast.SrcAllotment _, _ -> failwith "[TODO] impl allot"
;;

let compile_dest ~pulled_amt_reg ctx = function
  | Ast.DestAccount account_expr ->
    let account = compile_expr ctx account_expr in
    push_instruction ctx (Virtual_instruction.SendToAccount { account; cap = None });
    Ok ()
  | Ast.DestAllotment _ -> failwith "[TODO] impl allotment dest"
  | Ast.DestInorder _ -> failwith "[TODO] impl inorder dest"
;;

let compile_stmt ctx =
  let ( let* ) = Result.bind in
  function
  | Ast.StmtSend { monetary; source; destination } ->
    let pulled_amt_reg =
      push_instruction_dest ctx (fun dest ->
        Virtual_instruction.LoadConst { value = `Int 0L; dest })
    in
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
    let* () = compile_source ~pulled_amt_reg ~cap_reg:(Some cap_reg) ctx source in
    let* () = compile_dest ~pulled_amt_reg ctx destination in
    Ok ()
  | Ast.StmtSendAll { asset; source; destination } ->
    let pulled_amt_reg =
      push_instruction_dest ctx (fun dest ->
        Virtual_instruction.LoadConst { value = `Int 0L; dest })
    in
    let asset_reg = compile_expr ctx asset in
    push_instruction ctx (Virtual_instruction.SetCurrentAsset { asset = asset_reg });
    let* () = compile_source ~pulled_amt_reg ~cap_reg:None ctx source in
    let* () = compile_dest ~pulled_amt_reg ctx destination in
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

let test_compiled source =
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
  Format.printf "%a" Virtual_instruction.pp_program compiled_program.instructions
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
    $r0 <- load_const(0)
    $r2 <- load_const("USD/2")
    $r3 <- load_const(10)
    $r1 <- mk_monetary($r2, $r3)
    $r4 <- get_asset($r1)
    set_current_asset($r4)
    $r5 <- get_amount($r1)
    $r6 <- load_const("src")
    $r0 <- pull_account($r6, $r5)
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
    $r0 <- load_const(0)
    $r2 <- load_const("USD/2")
    $r3 <- load_const(10)
    $r1 <- mk_monetary($r2, $r3)
    $r4 <- get_asset($r1)
    set_current_asset($r4)
    $r5 <- get_amount($r1)
    $r7 <- int_copy($r5)
    $r9 <- load_const("s1")
    $r8 <- pull_account($r9, $r7)
    $r0 <- add_int($r0, $r8)
    $r7 <- sub_int($r7, $r8)
    jmp_if_zero($r7, #inorder_end)
    $r11 <- load_const("s2")
    $r10 <- pull_account($r11, $r7)
    $r0 <- add_int($r0, $r10)
    #inorder_end
    $r12 <- load_const("dest")
    send_to_account_uncapped($r12)
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
    $r0 <- load_const(0)
    $r2 <- load_const("USD/2")
    $r3 <- load_const(10)
    $r1 <- mk_monetary($r2, $r3)
    $r4 <- get_asset($r1)
    set_current_asset($r4)
    $r5 <- get_amount($r1)
    $r7 <- load_const("USD/2")
    $r8 <- load_const(5)
    $r6 <- mk_monetary($r7, $r8)
    $r9 <- get_amount($r6)
    $r10 <- min_int($r9, $r5)
    $r11 <- load_const("s1")
    $r0 <- pull_account($r11, $r10)
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
    $r0 <- load_const(0)
    $r1 <- load_const("USD/2")
    set_current_asset($r1)
    $r3 <- load_const("USD/2")
    $r4 <- load_const(5)
    $r2 <- mk_monetary($r3, $r4)
    $r5 <- get_amount($r2)
    $r6 <- load_const("s1")
    $r0 <- pull_account($r6, $r5)
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
    $r0 <- load_const(0)
    $r2 <- load_const("USD/2")
    $r3 <- load_const(10)
    $r1 <- mk_monetary($r2, $r3)
    $r4 <- get_asset($r1)
    set_current_asset($r4)
    $r5 <- get_amount($r1)
    $r7 <- int_copy($r5)
    $r10 <- load_const("USD/2")
    $r11 <- load_const(5)
    $r9 <- mk_monetary($r10, $r11)
    $r12 <- get_amount($r9)
    $r13 <- min_int($r12, $r7)
    $r14 <- load_const("s1")
    $r8 <- pull_account($r14, $r13)
    $r0 <- add_int($r0, $r8)
    $r7 <- sub_int($r7, $r8)
    jmp_if_zero($r7, #inorder_end)
    $r16 <- load_const("s2")
    $r15 <- pull_account($r16, $r7)
    $r0 <- add_int($r0, $r15)
    #inorder_end
    $r17 <- load_const("dest")
    send_to_account_uncapped($r17)
    |}]
;;
