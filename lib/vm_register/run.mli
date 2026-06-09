val run_program
  :  vars:string Common.Run_state.StringMap.t
  -> balances:int64 Common.Run_state.PairsMap.t
  -> Syntax.Ast.program
  -> ( Common.posting list
       , [> `Compilation of Common.compilation_err | `Runtime of Common.run_error ] )
       result
