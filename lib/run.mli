include module type of Run_intf

val run_program
  :  vars:string StringMap.t
  -> balances:int PairsMap.t
  -> Ast.program
  -> posting list
