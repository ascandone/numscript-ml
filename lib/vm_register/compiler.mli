include module type of Compiler_intf

val compile_parsed : Syntax.Ast.program -> (compiled_program, compilation_err) result
