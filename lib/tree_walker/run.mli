include module type of Run_intf

val run_program
  :  vars:string StringMap.t
  -> balances:int64 PairsMap.t
  -> Syntax.Ast.program
  -> (Common.posting list, [> `Runtime of Common.run_error ]) result
