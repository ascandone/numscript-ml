include module type of Run_intf

val run_program
  :  vars:string StringMap.t
  -> balances:int64 PairsMap.t
  -> Syntax.Ast.program
  -> ( posting list
       , [> `Compilation of Compiler.compilation_err | `Runtime of run_error ] )
       result
