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

let send ctx name amt =
  let current_bal = get_account_balance ctx name in
  Hashtbl.replace ctx.balances (name, !(ctx.current_asset)) (Int64.sub current_bal amt);
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
      if avl_amt >= dest_cap
      then (
        add dest_cap;
        let diff = Int64.sub avl_amt dest_cap in
        if diff > 0L then add_left ctx.sources (source, diff))
      else (
        add avl_amt;
        send_to_acc ctx destination ~dest_cap:(Int64.sub dest_cap avl_amt)))
;;
