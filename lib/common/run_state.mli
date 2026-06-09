include module type of Run_state_intf

val create : unit -> run_state
val set_balances : run_state -> int64 PairsMap.t -> unit
val get_account_balance : run_state -> string -> int64

(* source *)
val pull
  :  ?overdraft_bound:int64 option
  -> source:string
  -> cap:int64
  -> run_state
  -> int64

val pull_uncapped : ?overdraft_bound:int64 -> source:string -> run_state -> int64

(* destination *)
val send : dest:string -> cap:int64 -> run_state -> unit
val send_uncapped : dest:string -> run_state -> unit
