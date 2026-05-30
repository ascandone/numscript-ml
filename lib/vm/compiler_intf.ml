type compilation_err =
  | InvalidFn of { fn_name : string }
  | BadArity of
      { fn_name : string
      ; received : int
      }
