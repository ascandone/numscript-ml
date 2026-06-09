open Common

type stacks =
  { string_like : string Stack.t
  ; int : int64 Stack.t
  ; monetary : (string * int64) Stack.t
  ; portion : Portion.t Stack.t
  }

type ctx =
  { current_asset : string
  ; vars : string Run_state.StringMap.t
  ; program : Program.t
  ; stacks : stacks
  ; pc : int ref
  ; run_state : Run_state.run_state
  }

let min_opt n = function
  | None -> n
  | Some n1 -> min n n1
;;

exception RunError of run_error

let parse_var ctx ~typ ~name ~value =
  match typ with
  | Common.ExprTyp_Account | Common.ExprTyp_Asset | Common.ExprTyp_String ->
    (* For those types, we don't need to parse anything *)
    Stack.push value ctx.stacks.string_like
  | Common.ExprTyp_Number ->
    let parsed_num =
      match Int64.of_string_opt value with
      | None -> raise (RunError (InvalidVarSyntax { typ; value }))
      | Some parsed_num -> parsed_num
    in
    Stack.push parsed_num ctx.stacks.int
  | Common.ExprTyp_Portion -> failwith "[TODO] impl portion parsing"
  | Common.ExprTyp_Monetary -> failwith "[TODO] impl monetary parsing"
;;

let eval_bytecode (ctx : ctx) =
  Array.iter
  @@ function
  | Program.Expr_FetchConst { pool; pool_idx } ->
    (match pool with
     | `StringLike ->
       let str = ctx.program.constant_pool.string_like.(pool_idx) in
       Stack.push str ctx.stacks.string_like
     | `Int ->
       let str = ctx.program.constant_pool.int.(pool_idx) in
       Stack.push str ctx.stacks.int)
  | Program.Expr_GetLocal _ -> failwith "[TODO] impl get local"
  | Program.Expr_NumNeg ->
    let arg = Stack.pop ctx.stacks.int in
    Stack.push (Int64.neg arg) ctx.stacks.int
  | Program.Expr_NumAdd ->
    let l = Stack.pop ctx.stacks.int in
    let r = Stack.pop ctx.stacks.int in
    Stack.push (Int64.add l r) ctx.stacks.int
  | Program.Expr_NumSub ->
    let l = Stack.pop ctx.stacks.int in
    let r = Stack.pop ctx.stacks.int in
    Stack.push (Int64.sub l r) ctx.stacks.int
  | Program.Expr_MkMonetary ->
    let asset = Stack.pop ctx.stacks.string_like in
    let amount = Stack.pop ctx.stacks.int in
    Stack.push (asset, amount) ctx.stacks.monetary
  | Program.Expr_MkPortion ->
    let num = Stack.pop ctx.stacks.int in
    let den = Stack.pop ctx.stacks.int in
    Stack.push (Portion.create ~num ~den) ctx.stacks.portion
  | Program.Expr_FetchVar { typ; name_idx } ->
    let name = Stack.pop ctx.stacks.string_like in
    let value =
      match Run_state.StringMap.find_opt name ctx.vars with
      | None -> raise (RunError (UnboundVar name))
      | Some value -> value
    in
    parse_var ctx ~typ ~name ~value
;;

let eval_expr_by_idx ctx expr_idx ~stack =
  let expr_chunk = ctx.program.expr_chunks.(expr_idx) in
  let expr_bytecode =
    (* Careful: we need to be clear about whether the size refers to bytes, instructions, or something else.
        The VM may hydrate the bytecode into something else and lose the byte repr

      *)
    Array.sub ctx.program.expr_bytecode expr_chunk.start_idx expr_chunk.size
  in
  eval_bytecode ctx expr_bytecode;
  let item = Stack.pop stack in
  assert (Stack.is_empty stack);
  item
;;

let int64_to_non_neg = max 0L

let rec pull_source ?cap ctx =
  let op = ctx.program.sources.(!(ctx.pc)) in
  incr ctx.pc;
  match op with
  | Program.Src_Account { account_expr_idx } ->
    (* TODO(bug) we this account might be implicitly unbounded (@world). We don't know that statically so we need to check that here *)

    (* -- parse *)
    let name = eval_expr_by_idx ctx account_expr_idx ~stack:ctx.stacks.string_like in
    (* -- eval *)
    (match cap with
     | None -> Run_state.pull_uncapped ctx.run_state ~source:name
     | Some cap -> Run_state.pull ctx.run_state ~source:name ~cap)
  | Program.Src_AccountUnbounded { account_expr_idx } ->
    (* -- parse *)
    let name = eval_expr_by_idx ctx account_expr_idx ~stack:ctx.stacks.string_like in
    (* -- eval *)
    let cap =
      match cap with
      | None ->
        (* TODO double check this branch is unreachable *)
        failwith "[unreachable] invalid unbounded source in unbounded mode"
      | Some cap -> cap
    in
    Run_state.pull ctx.run_state ~overdraft_bound:None ~source:name ~cap
  | Program.Src_AccountBoundedOverdraft { account_expr_idx; overdraft_expr_idx } ->
    (* -- parse *)
    let name = eval_expr_by_idx ctx account_expr_idx ~stack:ctx.stacks.string_like in
    let max_overdraft_asset, max_overdraft_amount =
      eval_expr_by_idx ctx overdraft_expr_idx ~stack:ctx.stacks.monetary
    in
    let max_overdraft_amount = int64_to_non_neg max_overdraft_amount in
    (* -- eval *)
    let acc_balance = Run_state.get_account_balance ctx.run_state name in
    let amt =
      match cap with
      | None -> int64_to_non_neg (Int64.add acc_balance max_overdraft_amount)
      | Some cap ->
        min cap (int64_to_non_neg (Int64.add acc_balance max_overdraft_amount))
    in
    Run_state.pull
      ~overdraft_bound:(Some max_overdraft_amount)
      ctx.run_state
      ~source:name
      ~cap:amt
  | Program.Src_Max { monetary_expr_idx } ->
    (* -- parse *)
    let _asset, max_cap =
      eval_expr_by_idx ctx monetary_expr_idx ~stack:ctx.stacks.monetary
    in
    (* TODO assert asset is current asset  *)
    (* -- eval *)
    pull_source ~cap:(int64_to_non_neg (min_opt max_cap cap)) ctx
  | Program.Src_Inorder { end_idx } ->
    let total_pulled = ref 0L in
    let cap_left_ref = ref cap in
    let should_keep_looping () =
      !(ctx.pc) < end_idx
      &&
      match !cap_left_ref with
      | None -> true
      | Some cap_left -> cap_left > 0L
    in
    while should_keep_looping () do
      let pulled = pull_source ?cap:!cap_left_ref ctx in
      total_pulled := Int64.add !total_pulled pulled;
      match !cap_left_ref with
      | None -> ()
      | Some previous_cap -> cap_left_ref := Some (Int64.sub previous_cap pulled)
    done;
    (* We make sure we deplete the inorder even if shortcircuited: *)
    ctx.pc := end_idx;
    !total_pulled
  | Program.Src_Allotment { array_const_idx } ->
    (* -- parse *)
    let portions_array = ctx.program.constant_pool.array.(array_const_idx) in
    let cap =
      match cap with
      | None -> failwith "[unreachable] allotment in unbounded mode"
      | Some cap -> cap
    in
    (* -- eval *)
    let portions_array =
      Array.map (eval_expr_by_idx ctx ~stack:ctx.stacks.portion) portions_array
    in
    Array.iter
      (fun needed_amt -> pull_source_amt ctx ~needed_amt)
      (calc_allot ~portions_array ~cap);
    cap

and pull_source_amt ctx ~needed_amt =
  let got_amt = pull_source ctx ~cap:needed_amt in
  if got_amt < needed_amt then raise (RunError MissingFunds);
  ()
;;

(* TODO cap should probably be an `int64 option` *)
let rec send_to_dest ctx ~cap =
  let op = ctx.program.destinations.(!(ctx.pc)) in
  incr ctx.pc;
  match op with
  | Program.Dest_Account { account_expr_idx } ->
    let account = eval_expr_by_idx ctx account_expr_idx ~stack:ctx.stacks.string_like in
    Run_state.send ctx.run_state ~dest:account ~cap
  | Program.Dest_Kept -> failwith "[TODO] impl kept"
  | Program.Dest_Max { monetary_expr_idx } ->
    (* TODO check asset *)
    let _asset, clause_cap =
      eval_expr_by_idx ctx monetary_expr_idx ~stack:ctx.stacks.monetary
    in
    let clause_cap = int64_to_non_neg clause_cap in
    send_to_dest ctx ~cap:(min cap clause_cap);
    if clause_cap < cap
    then
      (* We can avoid continuing if we depleted fundings in the dest within "max" clause *)
      send_to_dest ctx ~cap:(Int64.sub cap clause_cap)
;;

let eval_statement ctx = function
  | Program.Stmt_SendAll { asset_expr_idx; source_idx; destination_idx } ->
    let asset = eval_expr_by_idx ctx asset_expr_idx ~stack:ctx.stacks.string_like in
    let ctx = { ctx with current_asset = asset } in
    (* TODO update ctx with asset *)
    ctx.pc := source_idx;
    let pulled = pull_source ctx in
    ctx.pc := destination_idx;
    send_to_dest ctx ~cap:pulled
  | Program.Stmt_Send { monetary_expr_idx; source_idx; destination_idx } ->
    let asset, needed_amt =
      eval_expr_by_idx ctx monetary_expr_idx ~stack:ctx.stacks.monetary
    in
    let ctx = { ctx with current_asset = asset } in
    (* TODO update ctx with asset *)
    ctx.pc := source_idx;
    pull_source_amt ~needed_amt ctx;
    ctx.pc := destination_idx;
    send_to_dest ctx ~cap:needed_amt
  | Program.Stmt_Save { monetary_expr_idx; account_expr_idx } ->
    let asset, _needed_amt =
      eval_expr_by_idx ctx monetary_expr_idx ~stack:ctx.stacks.monetary
    in
    let ctx = { ctx with current_asset = asset } in
    let _account = eval_expr_by_idx ctx account_expr_idx ~stack:ctx.stacks.string_like in
    failwith "TODO save"
  | Program.Stmt_SetLocal _ -> failwith "TODO setVar"
  | Program.Stmt_FnSetAccountMeta -> failwith "TODO fn"
  | Program.Stmt_FnSetTxMeta -> failwith "TODO fn"
;;

let run_compiled ~vars ~balances (program : Program.t) =
  let empty_vm : stacks =
    { portion = Stack.create ()
    ; monetary = Stack.create ()
    ; int = Stack.create ()
    ; string_like = Stack.create ()
    }
  in
  let run_ctx : ctx =
    { current_asset = ""
    ; vars
    ; program
    ; stacks = empty_vm
    ; pc = ref 0
    ; run_state = Run_state.create ()
    }
  in
  Run_state.set_balances run_ctx.run_state balances;
  match program.statements |> Array.iter (eval_statement run_ctx) with
  | exception RunError e -> Error e
  | () -> Ok (run_ctx.run_state.postings |> Queue.to_seq |> List.of_seq)
;;

let run_program ~vars ~balances program =
  let ( let* ) = Result.bind in
  let* parsed_program =
    Compiler.compile_parsed program |> Result.map_error (fun e -> `Compilation e)
  in
  let* postings =
    run_compiled ~vars ~balances parsed_program |> Result.map_error (fun e -> `Runtime e)
  in
  Ok postings
;;
