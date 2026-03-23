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

(** Unbounded pull *)
let rec pull_all_source ctx = function
  | Ast_canonical.SrcAccount name ->
    let acc_balance = get_account_balance ctx name in
    send ctx name acc_balance
  | Ast_canonical.SrcMax (cap, sub_src) -> pull_source_capped ctx cap sub_src
  | Ast_canonical.SrcInorder srcs -> srcs |> List.map (pull_all_source ctx) |> lst_sum
  | Ast_canonical.SrcAllotment _ -> failwith "[unreachable] Forbidden in unbouded mode"

(** Bounded pull with cap. Doesn't fail if we don't reach the cap. *)
and pull_source_capped ctx cap = function
  | Ast_canonical.SrcAccount name ->
    let acc_balance = get_account_balance ctx name in
    send ctx name (min cap acc_balance)
  | Ast_canonical.SrcMax (max_cap, sub_src) ->
    pull_source_capped ctx (min cap max_cap) sub_src
  | Ast_canonical.SrcInorder srcs -> pull_sources_capped_inorder ctx cap srcs
  | Ast_canonical.SrcAllotment allot ->
    allot
    |> calc_allot cap
    |> List.map (fun (cap, src) -> pull_source_capped ctx cap src)
    |> lst_sum

and pull_sources_capped_inorder ctx cap = function
  | _ when cap <= 0 -> 0
  | [] -> 0
  | src :: sources ->
    let got_amt = pull_source_capped ctx cap src in
    got_amt + pull_sources_capped_inorder ctx (cap - got_amt) sources

(** Pull exactly the required amount, fail otherwise *)
and pull_amt ctx needed_amt src =
  let got_amt = pull_source_capped ctx needed_amt src in
  if got_amt < needed_amt then failwith "missing amt";
  got_amt
;;
