include Run_state_intf

let create () =
  { balances = Hashtbl.create 0
  ; sources = Dynarray.create ()
  ; postings = Queue.create ()
  ; current_asset = ref ""
  }
;;

let set_balances st balances =
  let balances = PairsMap.to_seq balances |> Hashtbl.of_seq in
  st.balances <- balances
;;

let get_account_balance state name =
  Hashtbl.find_opt state.balances (name, !(state.current_asset))
  |> Option.value ~default:0L
;;

let int64_to_non_neg = Int64.max 0L

let pull ?(overdraft_bound = Some 0L) ~source ~cap ctx =
  let cap = int64_to_non_neg cap in
  let current_bal = get_account_balance ctx source in
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

(** 
Pulls all of the source's balance
Can't be unbounded overdraft
*)
let pull_uncapped ?(overdraft_bound = 0L) ~source ctx =
  let current_bal = get_account_balance ctx source in
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

let rec send ~dest:destination ~cap ctx =
  if cap <= 0L
  then ()
  else (
    match pop_first_opt ctx.sources with
    | None -> ()
    | Some (source, avl_amt) ->
      let add amount =
        if amount > 0L
        then (
          Queue.add
            { Common_intf.source; destination; amount; asset = !(ctx.current_asset) }
            ctx.postings;
          let dst_bal =
            Hashtbl.find_opt ctx.balances (destination, !(ctx.current_asset))
            |> Option.value ~default:0L
          in
          Hashtbl.replace
            ctx.balances
            (destination, !(ctx.current_asset))
            (Int64.add dst_bal amount))
      in
      if avl_amt >= cap
      then (
        add cap;
        let diff = Int64.sub avl_amt cap in
        if diff > 0L then add_left ctx.sources (source, diff))
      else (
        add avl_amt;
        send ctx ~dest:destination ~cap:(Int64.sub cap avl_amt)))
;;

let rec send_uncapped ~dest:destination ctx =
  match pop_first_opt ctx.sources with
  | None -> ()
  | Some (source, avl_amt) ->
    if avl_amt > 0L
    then (
      Queue.add
        { Common_intf.source
        ; destination
        ; amount = avl_amt
        ; asset = !(ctx.current_asset)
        }
        ctx.postings;
      let dst_bal =
        Hashtbl.find_opt ctx.balances (destination, !(ctx.current_asset))
        |> Option.value ~default:0L
      in
      Hashtbl.replace
        ctx.balances
        (destination, !(ctx.current_asset))
        (Int64.add dst_bal avl_amt));
    send_uncapped ~dest:destination ctx
;;
