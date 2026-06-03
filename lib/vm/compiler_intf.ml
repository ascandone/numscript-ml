type compilation_err =
  | UnboundVar of string
  | InvalidType of string
  | InvalidFn of { fn_name : string }
  | BadArity of
      { fn_name : string
      ; received : int
      }
