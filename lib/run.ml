include Run_intf

type ctx =
  { current_asset : string
  ; balances : (string * string, int) Hashtbl.t
  ; sources : (string * int) Dynarray.t
  ; postings : posting Queue.t
  }

let create_ctx ~current_asset ~balances : ctx =
  { current_asset; balances; sources = Dynarray.create (); postings = Queue.create () }
;;

let get_account_balance ctx name =
  Hashtbl.find_opt ctx.balances (name, ctx.current_asset) |> Option.value ~default:0
;;

let send ctx name amt =
  let current_bal = get_account_balance ctx name in
  Hashtbl.replace ctx.balances (name, ctx.current_asset) (current_bal - amt);
  Dynarray.add_last ctx.sources (name, amt);
  amt
;;

let ceil_div x y = (x + y - 1) / y

(* TODO bug *)
let rec calc_allot amt = function
  | [] -> []
  | (Ast_canonical.Portion (num, denom), source) :: sources ->
    let ceil_val = ceil_div (amt * num) denom in
    (ceil_val, source) :: calc_allot (amt - ceil_val) sources
;;

let lst_sum = List.fold_left ( + ) 0

let min_opt n = function
  | None -> n
  | Some n1 -> min n n1
;;

(** Unbounded / Bounded pull with cap. Doesn't fail if we don't reach the cap. *)
let rec pull_source ctx ?cap = function
  | Ast_canonical.SrcAccount name ->
    let acc_balance = get_account_balance ctx name in
    send ctx name (min_opt acc_balance cap)
  | Ast_canonical.SrcMax (max_cap, sub_src) ->
    pull_source ctx ~cap:(min_opt max_cap cap) sub_src
  | Ast_canonical.SrcInorder srcs -> pull_sources_capped_inorder ctx ?cap srcs
  | Ast_canonical.SrcAllotment allot ->
    (match cap with
     | None -> failwith "[unreachable] Forbidden in unbouded mode"
     | Some cap ->
       allot
       |> calc_allot cap
       |> List.map (fun (cap, src) -> pull_source ctx ~cap src)
       |> lst_sum)

and pull_sources_capped_inorder ctx ?cap sources =
  match sources, cap with
  | _, Some cap when cap <= 0 -> 0
  | [], _ -> 0
  | src :: sources, _ ->
    let got_amt = pull_source ctx ?cap src in
    let new_cap_opt = Option.map (fun cap -> cap - got_amt) cap in
    got_amt + pull_sources_capped_inorder ctx ?cap:new_cap_opt sources

(** Pull exactly the required amount, fail otherwise *)
and pull_amt ctx ~needed_amt src =
  let got_amt = pull_source ctx ~cap:needed_amt src in
  if got_amt < needed_amt then failwith "missing amt";
  got_amt
;;

(* dumb, O(n) op for the POC *)
let add_left (da : 'a Dynarray.t) (x : 'a) : unit =
  let temp = Dynarray.to_list da in
  Dynarray.clear da;
  Dynarray.add_last da x;
  List.iter (fun item -> Dynarray.add_last da item) temp
;;

(* TODO handle kept *)
let send_to_acc ctx destination ~dest_cap =
  let add source amount =
    Queue.add { source; destination; amount; asset = ctx.current_asset } ctx.postings
  in
  match Dynarray.pop_last_opt ctx.sources with
  | None -> ()
  | Some (source, avl_amt) ->
    (match () with
     | () when avl_amt >= dest_cap ->
       add source dest_cap;
       let diff = avl_amt - dest_cap in
       if diff != 0 then add_left ctx.sources (source, diff)
     | () (*   avl_amt < dest_cap  *) ->
       add source avl_amt;
       ())
;;

let rec send_to_dest ctx ~amt_left = function
  | Ast_canonical.DestAccount acc -> send_to_acc ctx acc ~dest_cap:amt_left
  | Ast_canonical.DestInorder (dests, remaining) ->
    dests
    |> List.iter (fun (clause : Ast_canonical.dest_inorder_clause) ->
      send_to_kept_or_dest ctx clause.cap clause.dest);
    (* BUG: amt_left doesn't take prev sendings into account *)
    send_to_kept_or_dest ctx amt_left remaining
  | Ast_canonical.DestAllotment _ -> failwith "[TODO] DestAllotment"

and send_to_kept_or_dest ctx cap = function
  | Ast_canonical.Kept ->
    (* TODO kept *)
    ()
  | Ast_canonical.Dest dest -> send_to_dest ctx dest ~amt_left:cap
;;

let run_stmt ctx = function
  | Ast_canonical.StmtSend { asset; amount; source; destination } ->
    let ctx = { ctx with current_asset = asset } in
    let _amt_got = pull_amt ctx ~needed_amt:amount source in
    let () = send_to_dest ctx ~amt_left:amount destination in
    ()
  | Ast_canonical.StmtSendAll { asset; source; destination } ->
    let ctx = { ctx with current_asset = asset } in
    let amt_got = pull_source ctx source in
    let () = send_to_dest ctx ~amt_left:amt_got destination in
    ()
  | Ast_canonical.Save _ -> failwith "TODO save"
;;

let parse_var ~typ ~raw_value =
  match typ with
  | "account" -> Value.Account raw_value
  | "asset" -> Value.Asset raw_value
  | "string" -> Value.String raw_value
  | "number" -> Value.Int (int_of_string raw_value)
  | "monetary" ->
    (match String.split_on_char ' ' raw_value with
     | [ asset; amt ] -> Value.Monetary (asset, int_of_string amt)
     | _ -> failwith "Error: invalid monetary lit")
  | "portion" -> failwith "TODO parse portion"
  | _ -> failwith "Error: unimplemented typ"
;;

let run_program ~vars ~balances (program : Ast.program) =
  let run_ctx : ctx =
    { current_asset = ""
    ; balances = PairsMap.to_seq balances |> Hashtbl.of_seq
    ; sources = Dynarray.create ()
    ; postings = Queue.create ()
    }
  in
  let eval_ctx : unit Eval_ast.ctx = { vars = Hashtbl.create 10 } in
  program.vars
  |> List.to_seq
  |> Seq.map (fun (v : Ast.var) ->
    let value =
      match v.value with
      | None ->
        (match StringMap.find_opt v.name vars with
         | None -> failwith "Err: missing variable"
         | Some raw_value -> parse_var ~typ:v.typ ~raw_value)
      | Some value_init -> Eval_ast.eval_expr eval_ctx value_init
    in
    v.name, value)
  |> Seq.iter (fun (name, value) -> ());
  program.statements
  |> List.map (Eval_ast.eval_statement eval_ctx)
  |> List.iter (run_stmt run_ctx);
  run_ctx.postings |> Queue.to_seq |> List.of_seq
;;
