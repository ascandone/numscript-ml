include module type of Run_state_intf

val create : unit -> run_state
val set_balances : run_state -> int64 PairsMap.t -> unit
val get_account_balance : run_state -> string -> int64
val send : run_state -> string -> int64 -> int64
val send_to_acc : run_state -> string -> dest_cap:int64 -> unit
