include Run_state_intf

let create () =
  { balances = Hashtbl.create 0
  ; sources = Dynarray.create ()
  ; postings = Dynarray.create ()
  ; current_asset = ref ""
  }
;;

let set_current_asset st asset = st.current_asset := asset

let set_balances st balances =
  let balances = PairsMap.to_seq balances |> Hashtbl.of_seq in
  st.balances <- balances
;;

let get_account_balance ~account ?asset state =
  let asset = Option.value asset ~default:!(state.current_asset) in
  Hashtbl.find_opt state.balances (account, asset) |> Option.value ~default:0L
;;

let int64_to_non_neg = Int64.max 0L

let pull ?(overdraft_bound = Some 0L) ~source ~cap ctx =
  let cap = int64_to_non_neg cap in
  let current_bal = get_account_balance ~account:source ctx in
  let available_amount_to_pull =
    match overdraft_bound with
    | None -> cap
    | Some bound ->
      let bound = int64_to_non_neg bound in
      let effective_balance = Int64.add current_bal bound in
      let effective_balance = int64_to_non_neg effective_balance in
      Int64.min effective_balance cap
  in
  Hashtbl.replace
    ctx.balances
    (source, !(ctx.current_asset))
    (Int64.sub current_bal available_amount_to_pull);
  Dynarray.add_last ctx.sources (source, available_amount_to_pull);
  available_amount_to_pull
;;

let pull_uncapped ?(overdraft_bound = 0L) ~source ctx =
  let current_bal = get_account_balance ctx ~account:source in
  let available = Int64.max 0L (Int64.add current_bal overdraft_bound) in
  if available > 0L
  then (
    Hashtbl.replace
      ctx.balances
      (source, !(ctx.current_asset))
      (Int64.sub current_bal available);
    Dynarray.add_last ctx.sources (source, available));
  available
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

(* Append a posting, merging with the previous one when source, destination,
   and asset all match. Also updates destination balance. *)
let add_posting ctx ~source ~destination ~asset ~amount =
  if amount <= 0L
  then ()
  else (
    let n = Dynarray.length ctx.postings in
    let merged =
      if n = 0
      then false
      else (
        let last = Dynarray.get ctx.postings (n - 1) in
        if
          String.equal last.Common_intf.source source
          && String.equal last.Common_intf.destination destination
          && String.equal last.Common_intf.asset asset
        then (
          Dynarray.set
            ctx.postings
            (n - 1)
            { last with Common_intf.amount = Int64.add last.amount amount };
          true)
        else false)
    in
    if not merged
    then Dynarray.add_last ctx.postings { Common_intf.source; destination; amount; asset };
    let dst_bal =
      Hashtbl.find_opt ctx.balances (destination, asset) |> Option.value ~default:0L
    in
    Hashtbl.replace ctx.balances (destination, asset) (Int64.add dst_bal amount))
;;

let rec send ?dest ~cap ctx =
  if cap <= 0L
  then ()
  else (
    match pop_first_opt ctx.sources with
    | None -> ()
    | Some (source, avl_amt) ->
      let asset = !(ctx.current_asset) in
      let credit amount =
        match dest with
        | Some destination -> add_posting ctx ~source ~destination ~asset ~amount
        | None ->
          (* Kept: refund the source. Consume funding, no posting. *)
          if amount > 0L
          then (
            let src_bal =
              Hashtbl.find_opt ctx.balances (source, asset) |> Option.value ~default:0L
            in
            Hashtbl.replace ctx.balances (source, asset) (Int64.add src_bal amount))
      in
      if avl_amt >= cap
      then (
        credit cap;
        let diff = Int64.sub avl_amt cap in
        if diff > 0L then add_left ctx.sources (source, diff))
      else (
        credit avl_amt;
        send ctx ?dest ~cap:(Int64.sub cap avl_amt)))
;;

let rec send_uncapped ?dest ctx =
  match pop_first_opt ctx.sources with
  | None -> ()
  | Some (source, avl_amt) ->
    if avl_amt > 0L
    then (
      match dest with
      | Some destination ->
        add_posting ctx ~source ~destination ~asset:!(ctx.current_asset) ~amount:avl_amt
      | None ->
        let asset = !(ctx.current_asset) in
        let src_bal =
          Hashtbl.find_opt ctx.balances (source, asset) |> Option.value ~default:0L
        in
        Hashtbl.replace ctx.balances (source, asset) (Int64.add src_bal avl_amt));
    send_uncapped ?dest ctx
;;

let get_postings ctx = Dynarray.to_list ctx.postings
