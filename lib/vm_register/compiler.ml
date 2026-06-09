include Compiler_intf
open Syntax

type ctx =
  { instructions : Virtual_instruction.t Dynarray.t
  ; next_reg : int ref
  ; next_label_id : int ref
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

let push_instruction ctx instr = Dynarray.add_last ctx.instructions instr

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

let push_instruction ctx instr = Dynarray.add_last ctx.instructions instr

let push_instruction_dest ctx get_instr =
  let dest = get_fresh_dest ctx in
  push_instruction ctx (get_instr dest);
  dest
;;

let rec compile_source ~pulled_amt_reg ~cap_reg ctx (source : Ast.source) =
  match source, cap_reg with
  | Ast.SrcAccountOverdraft { max_overdraft = None; _ }, None ->
    failwith "compilation error: uncapped overdraft"
  | Ast.SrcAccountOverdraft _, _ -> failwith "[TODO] overdraft"
  | Ast.SrcAccount name, None ->
    let _account_reg = compile_expr ctx name in
    failwith "[TODO] uncapped pull"
  | Ast.SrcAccount name, Some cap ->
    let account = compile_expr ctx name in
    push_instruction
      ctx
      (Virtual_instruction.PullAccount { cap; account; dest = pulled_amt_reg })
  | Ast.SrcInorder _, None -> failwith "compilation err: uncapped inorder"
  | Ast.SrcInorder srcs, Some outer_cap_reg ->
    (* TODO collapse together nested inorders *)
    let end_label = get_next_label_id ctx ~prefix:"inorder_end" in
    let last_elem_index = List.length srcs - 1 in
    let inorder_cap =
      push_instruction_dest ctx (fun dest ->
        Virtual_instruction.UnaryOp { op = `int_copy; arg = outer_cap_reg; dest })
    in
    List.iteri
      (fun src_index src ->
         let inner_pulled_amt_reg = get_fresh_dest ctx in
         compile_source
           ~pulled_amt_reg:inner_pulled_amt_reg
           ~cap_reg:(Some inorder_cap)
           ctx
           src;
         push_instruction
           ctx
           (Virtual_instruction.BinaryOp
              { op = `add_int
              ; dest = pulled_amt_reg
              ; left = pulled_amt_reg
              ; right = inner_pulled_amt_reg
              });
         if src_index < last_elem_index
         then (
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
             (Virtual_instruction.JmpIfZero { value = inorder_cap; label = end_label })))
      srcs;
    push_instruction ctx (Virtual_instruction.Label end_label)
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
            { op = `int_min; left = clause_cap_int_reg; right = outer_cap_reg; dest })
    in
    compile_source ~pulled_amt_reg ~cap_reg:(Some cap_reg) ctx sub_src
  | Ast.SrcAllotment _, _ -> failwith "[TODO] impl allot"
;;

let compile_dest ~pulled_amt_reg ctx = function
  (* TODO *)
  | Ast.DestAccount account_expr ->
    let account = compile_expr ctx account_expr in
    push_instruction ctx (Virtual_instruction.SendToAccount { account; cap = None })
  | Ast.DestAllotment _ -> failwith "[TODO] impl allotment dest"
  | Ast.DestInorder _ -> failwith "[TODO] impl inorder dest"
;;

let compile_stmt ctx = function
  | Ast.StmtSend { monetary; source; destination } ->
    let pulled_amt_reg =
      push_instruction_dest ctx (fun dest ->
        Virtual_instruction.LoadConst { value = `Int 0L; dest })
    in
    let monetary_reg = compile_expr ctx monetary in
    let cap_reg =
      push_instruction_dest ctx (fun dest ->
        Virtual_instruction.UnaryOp { op = `get_amount; arg = monetary_reg; dest })
    in
    compile_source ~pulled_amt_reg ~cap_reg:(Some cap_reg) ctx source;
    compile_dest ~pulled_amt_reg ctx destination
  | Ast.StmtSendAll { asset; source; destination } ->
    let pulled_amt_reg =
      push_instruction_dest ctx (fun dest ->
        Virtual_instruction.LoadConst { value = `Int 0L; dest })
    in
    let _asset_reg = compile_expr ctx asset in
    compile_source ~pulled_amt_reg ~cap_reg:None ctx source;
    compile_dest ~pulled_amt_reg ctx destination
  | Ast.Save _ -> failwith "[TODO] compile stmt"
  | Ast.FnStatement _ -> failwith "[TODO] compile stmt"
;;

let compile_parsed (program : Ast.program) =
  let ctx : ctx =
    { instructions = Dynarray.create (); next_reg = ref 0; next_label_id = ref 0 }
  in
  List.iter (compile_stmt ctx) program.statements;
  let compiled : compiled_program =
    { instructions = Dynarray.to_array ctx.instructions }
  in
  Ok compiled
;;

let test_compiled source =
  let parsed_ast = Syntax.Parser.parse source in
  let result = compile_parsed parsed_ast in
  let compiled_program =
    match result with
    | Error e -> failwith (show_compilation_err e)
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
    $r4 <- get_amount($r1)
    $r5 <- load_const("src")
    $r0 <- pull_account($r5, $r4)
    $r6 <- load_const("dest")
    send_to_account_uncapped($r6)
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
    $r4 <- get_amount($r1)
    $r6 <- int_copy($r4)
    $r8 <- load_const("s1")
    $r7 <- pull_account($r8, $r6)
    $r0 <- add_int($r0, $r7)
    $r6 <- sub_int($r6, $r7)
    jmp_if_zero($r6, #inorder_end)
    $r10 <- load_const("s2")
    $r9 <- pull_account($r10, $r6)
    $r0 <- add_int($r0, $r9)
    #inorder_end
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
    $r0 <- load_const(0)
    $r2 <- load_const("USD/2")
    $r3 <- load_const(10)
    $r1 <- mk_monetary($r2, $r3)
    $r4 <- get_amount($r1)
    $r6 <- load_const("USD/2")
    $r7 <- load_const(5)
    $r5 <- mk_monetary($r6, $r7)
    $r8 <- get_amount($r5)
    $r9 <- int_min($r8, $r4)
    $r10 <- load_const("s1")
    $r0 <- pull_account($r10, $r9)
    $r11 <- load_const("dest")
    send_to_account_uncapped($r11)
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
    $r4 <- get_amount($r1)
    $r6 <- int_copy($r4)
    $r9 <- load_const("USD/2")
    $r10 <- load_const(5)
    $r8 <- mk_monetary($r9, $r10)
    $r11 <- get_amount($r8)
    $r12 <- int_min($r11, $r6)
    $r13 <- load_const("s1")
    $r7 <- pull_account($r13, $r12)
    $r0 <- add_int($r0, $r7)
    $r6 <- sub_int($r6, $r7)
    jmp_if_zero($r6, #inorder_end)
    $r15 <- load_const("s2")
    $r14 <- pull_account($r15, $r6)
    $r0 <- add_int($r0, $r14)
    #inorder_end
    $r16 <- load_const("dest")
    send_to_account_uncapped($r16)
    |}]
;;
