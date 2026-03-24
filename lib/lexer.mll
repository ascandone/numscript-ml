{
  open Grammar

  let strip_underscores s =
    String.concat "" (String.split_on_char '_' s)

  let parse_perc p =
    let num_str = String.sub p 0 (String.length p - 1) in
    int_of_float (float_of_string num_str)
}

let white = [' ' '\t' '\r' '\n']+
let digit = ['0'-'9']
let number = digit+ ('_' digit+)*
let perc = number ('.' digit+)? '%'

let var_name = '$' ['a'-'z' '_'] ['a'-'z' '0'-'9' '_']*
let account = '@' ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']+ (':' ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']+)*
let asset = ['A'-'Z'] ['A'-'Z' '0'-'9']* ('/' digit+)?

(* New regex for standard variable types (e.g. 'account', 'asset', 'string') *)
let identifier = ['a'-'z']+ ['a'-'z' '_']*

rule read = parse
  | white { read lexbuf }
  | "/*"  { multiline_comment lexbuf }
  | "//"  { line_comment lexbuf }

  (* Symbols *)
  | '+' { PLUS }
  | '-' { MINUS }
  | '/' { DIV }
  | '=' { EQ }      (* NEW *)
  | '*' { STAR }    (* NEW *)
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '(' { LPAREN }
  | ')' { RPAREN }

  (* Keywords *)
  | "max"         { MAX }
  | "from"        { FROM }
  | "to"          { TO }
  | "remaining"   { REMAINING }
  | "kept"        { KEPT }
  | "send"        { SEND }
  | "save"        { SAVE }
  | "vars"        { VARS }
  | "source"      { SOURCE }
  | "destination" { DESTINATION }

  (* Literals & Identifiers *)
  | identifier as id { IDENTIFIER id }  (* NEW: Must be below keywords! *)
  | var_name   as v  { VARIABLE v }
  | account    as a  { VARIABLE a }
  | asset      as a  { VARIABLE a }
  
  | number as i { INT (int_of_string (strip_underscores i)) }
  | perc   as p { PERC (parse_perc p) }
  
  | eof { EOF }
  | _   { failwith (Printf.sprintf "Unexpected character: %c" (Lexing.lexeme_char lexbuf 0)) }

and multiline_comment = parse
  | "*/" { read lexbuf }
  | _    { multiline_comment lexbuf }
  | eof  { failwith "Unterminated comment" }

and line_comment = parse
  | '\n' { read lexbuf }
  | _    { line_comment lexbuf }
  | eof  { EOF }