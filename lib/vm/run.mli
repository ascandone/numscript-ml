include module type of Run_intf

type run_error = MissingFunds

val run_program
  :  vars:string StringMap.t
  -> balances:int64 PairsMap.t
  -> Program.t
  -> (posting list, run_error) result
