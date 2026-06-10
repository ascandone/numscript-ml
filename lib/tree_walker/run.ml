open Common

exception RunError of Common.run_error

type ctx = Run_state.run_state

let calc_allot total portions_items =
  let portions = List.map fst portions_items in
  let items = List.map snd portions_items in
  let dens = List.map (fun (Ast_canonical.Portion (_, d)) -> d) portions in
  let lc = List.fold_left Internal_common.lcm 1L dens in
  let nums =
    List.map (fun (Ast_canonical.Portion (n, d)) -> Int64.mul n (Int64.div lc d)) portions
  in
  let bases = List.map (fun n -> Int64.div (Int64.mul n total) lc) nums in
  let leftover = Int64.sub total (List.fold_left Int64.add 0L bases) in
  let rec distribute extra bases items =
    match bases, items with
    | [], _ | _, [] -> []
    | b :: bs, x :: xs ->
      let amt = if Int64.compare extra 0L > 0 then Int64.add b 1L else b in
      (amt, x) :: distribute (Int64.sub extra 1L) bs xs
  in
  distribute leftover bases items
;;

let min_opt n = function
  | None -> n
  | Some n1 -> min n n1
;;

(** Unbounded / Bounded pull with cap. Doesn't fail if we don't reach the cap. *)
let rec pull_source ctx ?cap = function
  | Ast_canonical.SrcAccount name ->
    (match cap with
     | None -> Run_state.pull_uncapped ~source:name ctx
     | Some cap -> Run_state.pull ~cap ~source:name ctx)
  | Ast_canonical.SrcAccountOverdraft { account; max_overdraft } ->
    (match max_overdraft, cap with
     | None, None -> failwith "error: invalid uncapped unbounded"
     | Some max_overdraft, None -> Run_state.pull_uncapped ctx ~source:account
     | _, Some cap ->
       Run_state.pull
         ~overdraft_bound:(Option.map (fun (_, i) -> i) max_overdraft)
         ~cap
         ~source:account
         ctx)
  | Ast_canonical.SrcMax (max_cap, sub_src) ->
    pull_source ctx ~cap:(max 0L (min_opt max_cap cap)) sub_src
  | Ast_canonical.SrcInorder srcs -> pull_sources_capped_inorder ctx ?cap srcs
  | Ast_canonical.SrcAllotment allot ->
    (match cap with
     | None -> failwith "[unreachable] Forbidden in unbouded mode"
     | Some cap ->
       allot
       |> calc_allot cap
       |> List.map (fun (cap, src) -> pull_source ctx ~cap src)
       |> List.fold_left Int64.add 0L)

and pull_sources_capped_inorder ctx ?cap sources =
  match sources, cap with
  | _, Some cap when cap <= 0L -> 0L
  | [], _ -> 0L
  | src :: sources, _ ->
    let got_amt = pull_source ctx ?cap src in
    let new_cap_opt = Option.map (fun cap -> Int64.sub cap got_amt) cap in
    Int64.add got_amt (pull_sources_capped_inorder ctx ?cap:new_cap_opt sources)

(** Pull exactly the required amount, fail otherwise *)
and pull_amt ctx ~needed_amt src =
  let got_amt = pull_source ctx ~cap:needed_amt src in
  if got_amt < needed_amt then raise (RunError MissingFunds);
  got_amt
;;

let add_left (da : 'a Dynarray.t) (x : 'a) : unit =
  let temp = Dynarray.to_list da in
  Dynarray.clear da;
  Dynarray.add_last da x;
  List.iter (Dynarray.add_last da) temp
;;

let pop_first_opt (da : 'a Dynarray.t) : 'a option =
  match Dynarray.to_list da with
  | [] -> None
  | x :: rest ->
    Dynarray.clear da;
    List.iter (Dynarray.add_last da) rest;
    Some x
;;

let rec send_to_dest ctx ~amt_left = function
  | Ast_canonical.DestAccount acc -> Run_state.send ctx ~dest:acc ~cap:amt_left
  | Ast_canonical.DestInorder (dests, remaining) ->
    let amt_used =
      List.fold_left
        (fun used (clause : Ast_canonical.dest_inorder_clause) ->
           let actual_cap = min clause.cap (Int64.sub amt_left used) in
           send_to_kept_or_dest ctx actual_cap clause.dest;
           Int64.add used actual_cap)
        0L
        dests
    in
    send_to_kept_or_dest ctx (Int64.sub amt_left amt_used) remaining
  | Ast_canonical.DestAllotment allots ->
    List.iter
      (fun (cap, kd) -> send_to_kept_or_dest ctx cap kd)
      (calc_allot amt_left allots)

and send_to_kept_or_dest ctx cap = function
  | Ast_canonical.Kept ->
    let rec restore remaining = failwith "TODO kept" in
    restore cap
  | Ast_canonical.Dest dest -> send_to_dest ctx dest ~amt_left:cap
;;

let run_stmt (ctx : Run_state.run_state) = function
  | Ast_canonical.StmtSend { asset; amount; source; destination } ->
    Run_state.set_current_asset ctx asset;
    let _amt_got = pull_amt ctx ~needed_amt:amount source in
    send_to_dest ctx ~amt_left:amount destination
  | Ast_canonical.StmtSendAll { asset; source; destination } ->
    Run_state.set_current_asset ctx asset;
    let amt_got = pull_source ctx source in
    send_to_dest ctx ~amt_left:amt_got destination
  | Ast_canonical.Save _ -> failwith "TODO save"
;;

let parse_var ~typ ~raw_value =
  match typ with
  | "account" -> Value.Asset raw_value
  | "asset" -> Value.Asset raw_value
  | "string" -> Value.String raw_value
  | "number" -> Value.Int (Int64.of_string raw_value)
  | "monetary" ->
    (match String.split_on_char ' ' raw_value with
     | [ asset; amt ] -> Value.Monetary (asset, Int64.of_string amt)
     | _ -> failwith "Error: invalid monetary lit")
  | "portion" -> failwith "TODO parse portion"
  | _ -> failwith "Error: unimplemented typ"
;;

let run_program ~vars ~balances (program : Syntax.Ast.program) =
  let run_ctx : ctx = Run_state.create () in
  Run_state.set_balances run_ctx balances;
  let eval_ctx : unit Eval_ast.ctx =
    { vars = Hashtbl.create 10; state = Run_state.create () }
  in
  List.iter
    (fun (v : Syntax.Ast.var) ->
       let value =
         match v.value with
         | None ->
           (match Run_state.StringMap.find_opt v.name vars with
            | None -> failwith "Err: missing variable"
            | Some raw_value -> parse_var ~typ:v.typ ~raw_value)
         | Some e -> Eval_ast.eval_expr eval_ctx e
       in
       Hashtbl.add eval_ctx.vars v.name value)
    program.vars;
  match
    program.statements
    |> List.map (Eval_ast.eval_statement eval_ctx)
    |> List.iter (run_stmt run_ctx)
  with
  | exception RunError e -> Error (`Runtime e)
  | () -> Ok (Run_state.get_postings run_ctx)
;;
