open Common

val run_program
  :  vars:string Run_state.StringMap.t
  -> balances:int64 Run_state.PairsMap.t
  -> Syntax.Ast.program
  -> (Common.posting list, [> `Runtime of Common.run_error ]) result
