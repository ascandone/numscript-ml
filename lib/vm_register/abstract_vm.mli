open Common

type t

val create : instructions:Virtual_instruction.t array -> t

val run
  :  vars:string Run_state.StringMap.t
  -> balances:int64 Run_state.PairsMap.t
  -> t
  -> (Common.posting list, Common.run_error) result
