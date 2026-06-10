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
  ; vars : (string, int) Hashtbl.t
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

let rec compile_expr ctx =
  let ( let* ) = Result.bind in
  function
  | Ast.ExprVar var_name ->
    let* var_reg =
      Hashtbl.find_opt ctx.vars var_name
      |> Option.to_result ~none:(Common.UnboundVar var_name)
    in
    Ok var_reg
  | Ast.ExprAccount str | Ast.ExprAsset str | Ast.ExprString str ->
    let dest =
      push_instruction_dest ctx (fun dest ->
        Virtual_instruction.LoadConst { value = `String str; dest })
    in
    Ok dest
  | Ast.ExprInt n ->
    let dest =
      push_instruction_dest ctx (fun dest ->
        Virtual_instruction.LoadConst { value = `Int (Int64.of_int n); dest })
    in
    Ok dest
  | Ast.ExprMonetaryLit (asset, amount) -> compile_infix ~op:`mk_monetary ctx asset amount
  | Ast.ExprInfix (op, left, right) ->
    let op =
      match op with
      | Ast.Add -> `add_int
      | Ast.Sub -> `sub_int
      | Ast.Div -> `mk_portion
    in
    compile_infix ~op ctx left right
  | Ast.ExprPerc _ -> failwith "[TODO] perc"
  | Ast.ExprFnCall _ -> failwith "[TODO] fn call"

and compile_infix ~(op : Virtual_instruction.binary_op) ctx left right =
  let ( let* ) = Result.bind in
  let* left = compile_expr ctx left in
  let* right = compile_expr ctx right in
  let dest = get_fresh_dest ctx in
  push_instruction ctx @@ Virtual_instruction.BinaryOp { op; left; right; dest };
  Ok dest

and compile_expr_to ~dest ~(copy : [> `int_copy | `portion_copy ]) ctx expr =
  let ( let* ) = Result.bind in
  let* inner_dest = compile_expr ctx expr in
  push_instruction ctx (Virtual_instruction.UnaryOp { op = copy; arg = inner_dest; dest });
  Ok ()
;;

(** returns the register holding the total amount pulled  *)
let rec compile_source ~cap_reg ctx (source : Ast.source) =
  let ( let* ) = Result.bind in
  match source, cap_reg with
  | Ast.SrcAllotment _, None -> Error Common.UncappedAllotment
  | Ast.SrcAccountOverdraft { max_overdraft = None; _ }, None ->
    Error Common.UncappedUnboundedOverdraft
  | Ast.SrcAccountOverdraft { max_overdraft = None; account }, Some cap ->
    let* account = compile_expr ctx account in
    let dest =
      push_instruction_dest ctx (fun dest ->
        Virtual_instruction.PullAccountUnboundedOverdraft { cap; account; dest })
    in
    Ok dest
  | Ast.SrcAccountOverdraft { account; max_overdraft = Some max_overdraft }, cap ->
    let* account = compile_expr ctx account in
    let* overdraft_bound_monetary = compile_expr ctx max_overdraft in
    let overdraft_bound_int =
      push_instruction_dest ctx (fun dest ->
        Virtual_instruction.UnaryOp
          { op = `get_amount; arg = overdraft_bound_monetary; dest })
    in
    let dest =
      push_instruction_dest ctx (fun dest ->
        Virtual_instruction.PullAccount
          { cap; account; dest; overdraft = `Bounded overdraft_bound_int })
    in
    Ok dest
  | Ast.SrcAccount name, cap ->
    let* account = compile_expr ctx name in
    let dest = get_fresh_dest ctx in
    push_instruction
      ctx
      (Virtual_instruction.PullAccount { cap; account; dest; overdraft = `BoundedZero });
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
    let* clause_cap_monetary_reg = compile_expr ctx clause_cap in
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
  | Ast.SrcAllotment clauses, Some cap_reg ->
    (* First, we compile portions so that we can create the allotment array (using mk_allotment).
      No subsources yet! *)
    let portions_arr =
      (*
      We MUST make sure we get contiguous registers before we compile por exprs
      Interleaving those compilations would result in portions not being contiguous
      *)
      List.map (fun (por, _) -> por, get_fresh_dest ctx) clauses
    in
    let* () =
      iter_result
        (fun (por, por_reg) ->
           match por with
           | None -> failwith "[TODO] remaining clause in allot"
           | Some por_expr ->
             compile_expr_to ~copy:`portion_copy ~dest:por_reg ctx por_expr)
        portions_arr
    in
    let allots_regs =
      List.map (fun (_por, sub_src) -> sub_src, get_fresh_dest ctx) clauses
    in
    push_instruction
      ctx
      (Virtual_instruction.MkAllotment
         { dest_arr = List.map snd allots_regs
         ; portions_arr = List.map snd portions_arr
         ; amount = cap_reg
         });
    (* Now that we know how much should each clause be capped with, we compile the sub-sources *)
    let* () =
      iter_result
        (fun (subsrc, cap_reg) ->
           let* _dest_reg = compile_source_with_required_amt ~cap_reg ctx subsrc in
           Ok ())
        allots_regs
    in
    Ok cap_reg

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
    let* account = compile_expr ctx account_expr in
    let cap =
      if Int.equal pulled_amt_reg current_cap then None else Some pulled_amt_reg
    in
    push_instruction
      ctx
      (Virtual_instruction.SendToAccount { account = Some account; cap });
    Ok ()
  | Ast.DestInorder (clauses, remaining) ->
    let* () =
      iter_result
        (fun ({ cap; dest } : Ast.dest_inorder_clause) ->
           let* cap_monetary_reg = compile_expr ctx cap in
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
  | Ast.DestAllotment clauses ->
    (* -- dedup this part from src *)
    let portions_arr = List.map (fun (por, _) -> por, get_fresh_dest ctx) clauses in
    let* () =
      iter_result
        (fun (por, por_reg) ->
           match por with
           | None -> failwith "[TODO] remaining clause in allot"
           | Some por_expr ->
             compile_expr_to ~copy:`portion_copy ~dest:por_reg ctx por_expr)
        portions_arr
    in
    let allots_regs =
      List.map (fun (_por, sub_src) -> sub_src, get_fresh_dest ctx) clauses
    in
    push_instruction
      ctx
      (Virtual_instruction.MkAllotment
         { dest_arr = List.map snd allots_regs
         ; portions_arr = List.map snd portions_arr
         ; amount = current_cap
         });
    (* -- end dedup *)
    let* () =
      iter_result
        (fun (subdest, pulled_amt_reg) ->
           let* _dest_reg =
             compile_kept_or_dest ~pulled_amt_reg ~current_cap ctx subdest
           in
           Ok ())
        allots_regs
    in
    Ok ()

and compile_kept_or_dest ~pulled_amt_reg ~current_cap ctx = function
  | Ast.Dest account_expr -> compile_dest ~pulled_amt_reg ~current_cap ctx account_expr
  | Ast.Kept ->
    let cap =
      if Int.equal pulled_amt_reg current_cap then None else Some pulled_amt_reg
    in
    push_instruction ctx (Virtual_instruction.SendToAccount { account = None; cap });
    Ok ()
;;

let compile_stmt ctx =
  let ( let* ) = Result.bind in
  function
  | Ast.StmtSend { monetary; source; destination } ->
    let* monetary_reg = compile_expr ctx monetary in
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
    let* asset_reg = compile_expr ctx asset in
    push_instruction ctx (Virtual_instruction.SetCurrentAsset { asset = asset_reg });
    let* pulled_amt_reg = compile_source ~cap_reg:None ctx source in
    let* () = compile_dest ~pulled_amt_reg ~current_cap:pulled_amt_reg ctx destination in
    Ok ()
  | Ast.Save _ -> failwith "[TODO] compile stmt"
  | Ast.FnStatement _ -> failwith "[TODO] compile stmt"
;;

let compile_var_def ctx (var_ : Ast.var) =
  let ( let* ) = Result.bind in
  let* typ =
    Common.parse_typ var_.typ |> Option.to_result ~none:(Common.InvalidType var_.typ)
  in
  (* check var is not shadowing another one *)
  let* () =
    match Hashtbl.find_opt ctx.vars var_.name with
    | None -> Ok ()
    | Some _previous_lookup -> Error (Common.DuplicateVar var_.name)
  in
  let* dest =
    match var_.value with
    | Some var_def -> compile_expr ctx var_def
    | None ->
      let dest =
        push_instruction_dest ctx (fun dest ->
          Virtual_instruction.FetchVariable { dest; typ; name = var_.name })
      in
      Ok dest
  in
  Hashtbl.replace ctx.vars var_.name dest;
  Ok ()
;;

let compile_parsed (program : Ast.program) =
  let ( let* ) = Result.bind in
  let ctx : ctx =
    { instructions = Dynarray.create ()
    ; next_reg = ref 0
    ; next_label_id = ref 0
    ; typecheck_state = Typecheck_instruction.create_state ()
    ; vars = Hashtbl.create 4
    }
  in
  let* () = iter_result (compile_var_def ctx) program.vars in
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
    $r0 <- load_const("USD/2")
    $r1 <- load_const(10)
    $r2 <- mk_monetary($r0, $r1)
    $r3 <- get_asset($r2)
    set_current_asset($r3)
    $r4 <- get_amount($r2)
    $r5 <- load_const("src")
    $r6 <- pull_account(account: $r5, cap: $r4)
    check_enough_funds($r6, $r4)
    $r7 <- load_const("dest")
    send_to_account($r7)
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
    $r6 <- pull_account(account: $r5, cap: $r4)
    check_enough_funds($r6, $r4)
    $r7 <- load_const("dest")
    send_to_account($r7)
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
    $r0 <- load_const("USD/2")
    $r1 <- load_const(10)
    $r2 <- mk_monetary($r0, $r1)
    $r3 <- get_asset($r2)
    set_current_asset($r3)
    $r4 <- get_amount($r2)
    $r5 <- load_const(0)
    $r6 <- int_copy($r4)
    $r7 <- load_const("s1")
    $r8 <- pull_account(account: $r7, cap: $r6)
    $r5 <- add_int($r5, $r8)
    $r6 <- sub_int($r6, $r8)
    jmp_if_zero($r6, #inorder_end)
    $r9 <- load_const("s2")
    $r10 <- pull_account(account: $r9, cap: $r6)
    $r5 <- add_int($r5, $r10)
    #inorder_end
    check_enough_funds($r5, $r4)
    $r11 <- load_const("dest")
    send_to_account($r11)
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
    $r0 <- load_const("USD/2")
    $r1 <- load_const(10)
    $r2 <- mk_monetary($r0, $r1)
    $r3 <- get_asset($r2)
    set_current_asset($r3)
    $r4 <- get_amount($r2)
    $r5 <- load_const("USD/2")
    $r6 <- load_const(5)
    $r7 <- mk_monetary($r5, $r6)
    $r8 <- get_amount($r7)
    $r9 <- min_int($r8, $r4)
    $r10 <- load_const("s1")
    $r11 <- pull_account(account: $r10, cap: $r9)
    check_enough_funds($r11, $r4)
    $r12 <- load_const("dest")
    send_to_account($r12)
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
    $r1 <- load_const("USD/2")
    $r2 <- load_const(5)
    $r3 <- mk_monetary($r1, $r2)
    $r4 <- get_amount($r3)
    $r5 <- load_const("s1")
    $r6 <- pull_account(account: $r5, cap: $r4)
    $r7 <- load_const("dest")
    send_to_account($r7)
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
    $r0 <- load_const("USD/2")
    $r1 <- load_const(10)
    $r2 <- mk_monetary($r0, $r1)
    $r3 <- get_asset($r2)
    set_current_asset($r3)
    $r4 <- get_amount($r2)
    $r5 <- load_const(0)
    $r6 <- int_copy($r4)
    $r7 <- load_const("USD/2")
    $r8 <- load_const(5)
    $r9 <- mk_monetary($r7, $r8)
    $r10 <- get_amount($r9)
    $r11 <- min_int($r10, $r6)
    $r12 <- load_const("s1")
    $r13 <- pull_account(account: $r12, cap: $r11)
    $r5 <- add_int($r5, $r13)
    $r6 <- sub_int($r6, $r13)
    jmp_if_zero($r6, #inorder_end)
    $r14 <- load_const("s2")
    $r15 <- pull_account(account: $r14, cap: $r6)
    $r5 <- add_int($r5, $r15)
    #inorder_end
    check_enough_funds($r5, $r4)
    $r16 <- load_const("dest")
    send_to_account($r16)
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
    $r13 <- pull_account(account: $r12, cap: $r11)
    $r5 <- add_int($r5, $r13)
    $r6 <- sub_int($r6, $r13)
    jmp_if_zero($r6, #inorder_end)
    $r14 <- load_const("s2")
    $r15 <- pull_account(account: $r14, cap: $r6)
    $r5 <- add_int($r5, $r15)
    #inorder_end
    check_enough_funds($r5, $r4)
    $r16 <- load_const("dest")
    send_to_account($r16)
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
    $r0 <- load_const("USD/2")
    $r1 <- load_const(10)
    $r2 <- mk_monetary($r0, $r1)
    $r3 <- get_asset($r2)
    set_current_asset($r3)
    $r4 <- get_amount($r2)
    $r5 <- load_const("src")
    $r6 <- pull_account(account: $r5, cap: $r4)
    check_enough_funds($r6, $r4)
    $r7 <- load_const("dest")
    send_to_account($r7)
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
    $r0 <- load_const("USD/2")
    $r1 <- load_const(10)
    $r2 <- mk_monetary($r0, $r1)
    $r3 <- get_asset($r2)
    set_current_asset($r3)
    $r4 <- get_amount($r2)
    $r5 <- load_const("src")
    $r6 <- pull_account(account: $r5, cap: $r4)
    check_enough_funds($r6, $r4)
    $r7 <- load_const("USD/2")
    $r8 <- load_const(5)
    $r9 <- mk_monetary($r7, $r8)
    $r10 <- get_amount($r9)
    $r11 <- min_int($r6, $r10)
    $r12 <- load_const("dest:capped")
    send_to_account($r12, cap: $r11)
    $r13 <- load_const("dest")
    send_to_account($r13)
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
    $r2 <- pull_account(account: $r1)
    $r3 <- load_const("dest")
    send_to_account($r3)
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
    $r3 <- pull_account(account: $r2)
    $r1 <- add_int($r1, $r3)
    $r4 <- load_const("src2")
    $r5 <- pull_account(account: $r4)
    $r1 <- add_int($r1, $r5)
    $r6 <- load_const("dest")
    send_to_account($r6)
    |}]
;;

let%expect_test "unbounded overdraft" =
  test_compiled
    {|
    send [USD/2 42] (
      source = @src allowing unbounded overdraft
      destination = @dest
    )
  |};
  [%expect
    {|
    $r0 <- load_const("USD/2")
    $r1 <- load_const(42)
    $r2 <- mk_monetary($r0, $r1)
    $r3 <- get_asset($r2)
    set_current_asset($r3)
    $r4 <- get_amount($r2)
    $r5 <- load_const("src")
    $r6 <- pull_account_unbounded_overdraft(account: $r5, cap: $r4)
    check_enough_funds($r6, $r4)
    $r7 <- load_const("dest")
    send_to_account($r7)
    |}]
;;

let%expect_test "bounded overdraft (capped)" =
  test_compiled
    {|
    send [USD/2 42] (
      source = @src allowing overdraft up to [USD/2 5]
      destination = @dest
    )
  |};
  [%expect
    {|
    $r0 <- load_const("USD/2")
    $r1 <- load_const(42)
    $r2 <- mk_monetary($r0, $r1)
    $r3 <- get_asset($r2)
    set_current_asset($r3)
    $r4 <- get_amount($r2)
    $r5 <- load_const("src")
    $r6 <- load_const("USD/2")
    $r7 <- load_const(5)
    $r8 <- mk_monetary($r6, $r7)
    $r9 <- get_amount($r8)
    $r10 <- pull_account(account: $r5, cap: $r4, overdraft: $r9)
    check_enough_funds($r10, $r4)
    $r11 <- load_const("dest")
    send_to_account($r11)
    |}]
;;

let%expect_test "bounded overdraft (uncapped)" =
  test_compiled
    {|
    send [USD/2 *] (
      source = @src allowing overdraft up to [USD/2 5]
      destination = @dest
    )
  |};
  [%expect
    {|
    $r0 <- load_const("USD/2")
    set_current_asset($r0)
    $r1 <- load_const("src")
    $r2 <- load_const("USD/2")
    $r3 <- load_const(5)
    $r4 <- mk_monetary($r2, $r3)
    $r5 <- get_amount($r4)
    $r6 <- pull_account(account: $r1, overdraft: $r5)
    $r7 <- load_const("dest")
    send_to_account($r7)
    |}]
;;

let%expect_test "allotment src" =
  test_compiled
    {|
    send [USD/2 10] (
      source = {
        1/3 from @s1
        2/3 from @s2
      }
      destination = @dest
    )
  |};
  [%expect
    {|
    $r0 <- load_const("USD/2")
    $r1 <- load_const(10)
    $r2 <- mk_monetary($r0, $r1)
    $r3 <- get_asset($r2)
    set_current_asset($r3)
    $r4 <- get_amount($r2)
    $r7 <- load_const(1)
    $r8 <- load_const(3)
    $r9 <- mk_portion($r7, $r8)
    $r5 <- portion_copy($r9)
    $r10 <- load_const(2)
    $r11 <- load_const(3)
    $r12 <- mk_portion($r10, $r11)
    $r6 <- portion_copy($r12)
    [$r13, $r14] <- mk_allot($r4, [$r5, $r6])
    $r15 <- load_const("s1")
    $r16 <- pull_account(account: $r15, cap: $r13)
    check_enough_funds($r16, $r13)
    $r17 <- load_const("s2")
    $r18 <- pull_account(account: $r17, cap: $r14)
    check_enough_funds($r18, $r14)
    check_enough_funds($r4, $r4)
    $r19 <- load_const("dest")
    send_to_account($r19)
    |}]
;;

let%expect_test "allotment dest" =
  test_compiled
    {|
    send [USD/2 10] (
      source = @src
      destination = {
        1/4 to @d1
        3/4 to @d2
      }
    )
  |};
  [%expect
    {|
    $r0 <- load_const("USD/2")
    $r1 <- load_const(10)
    $r2 <- mk_monetary($r0, $r1)
    $r3 <- get_asset($r2)
    set_current_asset($r3)
    $r4 <- get_amount($r2)
    $r5 <- load_const("src")
    $r6 <- pull_account(account: $r5, cap: $r4)
    check_enough_funds($r6, $r4)
    $r9 <- load_const(1)
    $r10 <- load_const(4)
    $r11 <- mk_portion($r9, $r10)
    $r7 <- portion_copy($r11)
    $r12 <- load_const(3)
    $r13 <- load_const(4)
    $r14 <- mk_portion($r12, $r13)
    $r8 <- portion_copy($r14)
    [$r15, $r16] <- mk_allot($r6, [$r7, $r8])
    $r17 <- load_const("d1")
    send_to_account($r17, cap: $r15)
    $r18 <- load_const("d2")
    send_to_account($r18, cap: $r16)
    |}]
;;

let%expect_test "internal vars" =
  test_compiled
    {|
    vars { account $acc = @acc }

    send [USD/2 10] (
      source = $acc
      destination = @dest
    )
  |};
  [%expect
    {|
    $r0 <- load_const("acc")
    $r1 <- load_const("USD/2")
    $r2 <- load_const(10)
    $r3 <- mk_monetary($r1, $r2)
    $r4 <- get_asset($r3)
    set_current_asset($r4)
    $r5 <- get_amount($r3)
    $r6 <- pull_account(account: $r0, cap: $r5)
    check_enough_funds($r6, $r5)
    $r7 <- load_const("dest")
    send_to_account($r7)
    |}]
;;

let%expect_test "extern vars" =
  test_compiled
    {|
    vars { account $acc }

    send [USD/2 10] (
      source = $acc
      destination = @dest
    )
  |};
  [%expect
    {|
    fetch_var<string>(acc)
    $r1 <- load_const("USD/2")
    $r2 <- load_const(10)
    $r3 <- mk_monetary($r1, $r2)
    $r4 <- get_asset($r3)
    set_current_asset($r4)
    $r5 <- get_amount($r3)
    $r6 <- pull_account(account: $r0, cap: $r5)
    check_enough_funds($r6, $r5)
    $r7 <- load_const("dest")
    send_to_account($r7)
    |}]
;;

let%expect_test "kept dest" =
  test_compiled
    {|
    send [USD/2 10] (
      source = @acc
      destination = {
        remaining kept
      }
    )
  |};
  [%expect
    {|
    $r0 <- load_const("USD/2")
    $r1 <- load_const(10)
    $r2 <- mk_monetary($r0, $r1)
    $r3 <- get_asset($r2)
    set_current_asset($r3)
    $r4 <- get_amount($r2)
    $r5 <- load_const("acc")
    $r6 <- pull_account(account: $r5, cap: $r4)
    check_enough_funds($r6, $r4)
    kept()
    |}]
;;
