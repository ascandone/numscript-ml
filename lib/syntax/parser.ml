let parse input =
  let lexbuf = Lexing.from_string input in
  Grammar.parse_program Lexer.read lexbuf
;;
