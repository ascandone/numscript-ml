include Run_intf

type stacks =
  { string_like : string Stack.t
  ; int : int64 Stack.t
  ; monetary : (string * int64) Stack.t
  ; portion : (int64 * int64) Stack.t
  }

type ctx =
  { current_asset : string
  ; balances : (string * string, int64) Hashtbl.t
  ; vars : string StringMap.t
  ; sources : (string * int64) Dynarray.t
  ; postings : posting Queue.t
  ; program : Program.t
  ; stacks : stacks
  ; pc : int ref
  }

let min_opt n = function
  | None -> n
  | Some n1 -> min n n1
;;

let get_account_balance ctx name =
  Hashtbl.find_opt ctx.balances (name, ctx.current_asset) |> Option.value ~default:0L
;;

let send ctx name amt =
  let current_bal = get_account_balance ctx name in
  Hashtbl.replace ctx.balances (name, ctx.current_asset) (Int64.sub current_bal amt);
  Dynarray.add_last ctx.sources (name, amt);
  amt
;;

let pop_first_opt (da : 'a Dynarray.t) : 'a option =
  match Dynarray.to_list da with
  | [] -> None
  | x :: rest ->
    Dynarray.clear da;
    List.iter (Dynarray.add_last da) rest;
    Some x
;;

let add_left (da : 'a Dynarray.t) (x : 'a) : unit =
  let temp = Dynarray.to_list da in
  Dynarray.clear da;
  Dynarray.add_last da x;
  List.iter (Dynarray.add_last da) temp
;;

let rec send_to_acc ctx destination ~dest_cap =
  if dest_cap <= 0L
  then ()
  else (
    match pop_first_opt ctx.sources with
    | None -> ()
    | Some (source, avl_amt) ->
      let add amount =
        if amount > 0L
        then (
          Queue.add
            { source; destination; amount; asset = ctx.current_asset }
            ctx.postings;
          let dst_bal =
            Hashtbl.find_opt ctx.balances (destination, ctx.current_asset)
            |> Option.value ~default:0L
          in
          Hashtbl.replace
            ctx.balances
            (destination, ctx.current_asset)
            (Int64.add dst_bal amount))
      in
      if avl_amt >= dest_cap
      then (
        add dest_cap;
        let diff = Int64.sub avl_amt dest_cap in
        if diff > 0L then add_left ctx.sources (source, diff))
      else (
        add avl_amt;
        send_to_acc ctx destination ~dest_cap:(Int64.sub dest_cap avl_amt)))
;;

exception RunError of run_error

let parse_var ctx ~typ ~name ~value =
  match typ with
  | Program.ExprTyp_Account | Program.ExprTyp_Asset | Program.ExprTyp_String ->
    (* For those types, we don't need to parse anything *)
    Stack.push value ctx.stacks.string_like
  | Program.ExprTyp_Number ->
    let parsed_num =
      match Int64.of_string_opt value with
      | None -> raise (RunError (InvalidVarSyntax { typ; value }))
      | Some parsed_num -> parsed_num
    in
    Stack.push parsed_num ctx.stacks.int
  | Program.ExprTyp_Portion -> failwith "[TODO] impl portion parsing"
  | Program.ExprTyp_Monetary -> failwith "[TODO] impl monetary parsing"
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
    let l = Stack.pop ctx.stacks.int in
    let r = Stack.pop ctx.stacks.int in
    Stack.push (l, r) ctx.stacks.portion
  | Program.Expr_FetchVar { typ; name_idx } ->
    let name = Stack.pop ctx.stacks.string_like in
    let value =
      match StringMap.find_opt name ctx.vars with
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

let rec pull_source ?cap ctx =
  let op = ctx.program.sources.(!(ctx.pc)) in
  incr ctx.pc;
  match op with
  | Program.Src_Account { account_expr_idx } ->
    (* TODO(bug) we this account might be implicitly unbounded (@world). We don't know that statically so we need to check that here *)

    (* -- parse *)
    let name = eval_expr_by_idx ctx account_expr_idx ~stack:ctx.stacks.string_like in
    (* -- eval *)
    let acc_balance = get_account_balance ctx name in
    send ctx name (min_opt (max 0L acc_balance) cap)
  | Program.Src_AccountUnbounded { account_expr_idx } ->
    (* -- parse *)
    let name = eval_expr_by_idx ctx account_expr_idx ~stack:ctx.stacks.string_like in
    (* -- eval *)
    let amt =
      match cap with
      | None ->
        (* TODO double check this branch is unreachable *)
        failwith "[unreachable] invalid unbounded source in unbounded mode"
      | Some cap -> cap
    in
    send ctx name amt
  | Program.Src_Max { monetary_expr_idx } ->
    (* -- parse *)
    let _asset, max_cap =
      eval_expr_by_idx ctx monetary_expr_idx ~stack:ctx.stacks.monetary
    in
    (* TODO assert asset is current asset  *)
    (* -- eval *)
    pull_source ~cap:(max 0L (min_opt max_cap cap)) ctx
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

and pull_source_amt ctx ~needed_amt =
  let got_amt = pull_source ctx ~cap:needed_amt in
  if got_amt < needed_amt then raise (RunError MissingFunds);
  got_amt
;;

let send_to_dest ctx ~cap =
  let op = ctx.program.destinations.(!(ctx.pc)) in
  incr ctx.pc;
  match op with
  | Program.Dest_Account { account_expr_idx } ->
    let account = eval_expr_by_idx ctx account_expr_idx ~stack:ctx.stacks.string_like in
    send_to_acc ctx account ~dest_cap:cap;
    ()
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
    let pulled = pull_source_amt ~needed_amt ctx in
    ctx.pc := destination_idx;
    send_to_dest ctx ~cap:pulled
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
    ; balances = PairsMap.to_seq balances |> Hashtbl.of_seq
    ; vars
    ; sources = Dynarray.create ()
    ; postings = Queue.create ()
    ; program
    ; stacks = empty_vm
    ; pc = ref 0
    }
  in
  match program.statements |> Array.iter (eval_statement run_ctx) with
  | exception RunError e -> Error e
  | () -> Ok (run_ctx.postings |> Queue.to_seq |> List.of_seq)
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
