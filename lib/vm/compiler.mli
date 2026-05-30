include module type of Compiler_intf

val compile_parsed : Syntax.Ast.program -> (Program.t, compilation_err) result
