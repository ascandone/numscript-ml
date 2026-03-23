type ctx =
  { current_asset : string
  ; vars : (string, Value.t) Hashtbl.t
  ; balances : (string * string, int) Hashtbl.t
  ; sources : (string * int) Queue.t
  }

let get_account_balance ctx name =
  Hashtbl.find_opt ctx.balances (name, ctx.current_asset) |> Option.value ~default:0
;;

let send ctx name amt =
  let current_bal = get_account_balance ctx name in
  Hashtbl.replace ctx.balances (name, ctx.current_asset) (current_bal - amt);
  Queue.push (name, amt) ctx.sources;
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
let rec pull_source ctx cap_opt = function
  | Ast_canonical.SrcAccount name ->
    let acc_balance = get_account_balance ctx name in
    send ctx name (min_opt acc_balance cap_opt)
  | Ast_canonical.SrcMax (max_cap, sub_src) ->
    pull_source ctx (Some (min_opt max_cap cap_opt)) sub_src
  | Ast_canonical.SrcInorder srcs -> pull_sources_capped_inorder ctx cap_opt srcs
  | Ast_canonical.SrcAllotment allot ->
    (match cap_opt with
     | None -> failwith "[unreachable] Forbidden in unbouded mode"
     | Some cap ->
       allot
       |> calc_allot cap
       |> List.map (fun (cap, src) -> pull_source ctx (Some cap) src)
       |> lst_sum)

and pull_sources_capped_inorder ctx cap_opt sources =
  match sources, cap_opt with
  | _, Some cap when cap <= 0 -> 0
  | [], _ -> 0
  | src :: sources, _ ->
    let got_amt = pull_source ctx cap_opt src in
    let new_cap_opt = Option.map (fun cap -> cap - got_amt) cap_opt in
    got_amt + pull_sources_capped_inorder ctx new_cap_opt sources

(** Pull exactly the required amount, fail otherwise *)
and pull_amt ctx needed_amt src =
  let got_amt = pull_sources_capped_inorder ctx (Some needed_amt) src in
  if got_amt < needed_amt then failwith "missing amt";
  got_amt
;;
