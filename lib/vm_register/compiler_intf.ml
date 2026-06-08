type compiled_program = { instructions : Virtual_instruction.t array }
[@@deriving show { with_path = false }]

type compilation_err =
  | UnboundVar of string
  | InvalidType of string
  | InvalidFn of { fn_name : string }
  | BadArity of
      { fn_name : string
      ; received : int
      }
[@@deriving show { with_path = false }]
