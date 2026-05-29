include module type of Run_intf

type run_error = MissingFunds

val run_program
  :  vars:string StringMap.t
  -> balances:int PairsMap.t
  -> Syntax.Ast.program
  -> (posting list, run_error) result
