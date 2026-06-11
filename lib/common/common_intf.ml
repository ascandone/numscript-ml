type expr_typ =
  | ExprTyp_Number
  | ExprTyp_String
  | ExprTyp_Account
  | ExprTyp_Asset
  | ExprTyp_Monetary
  | ExprTyp_Portion
[@@deriving show { with_path = false }]

type run_error =
  | MissingFunds
  | UnboundVar of string
  | BadVar of
      { typ : [ `Monetary | `Portion ]
      ; raw_value : string
      }
  | InvalidVarSyntax of
      { typ : expr_typ
      ; value : string
      }

type compilation_err =
  | UncappedUnboundedOverdraft
  | UncappedAllotment
  | DuplicateRemaining
  | DuplicateVar of string
  | UnboundVar of string
  | InvalidType of string
  | InvalidFn of { fn_name : string }
  | BadArity of
      { fn_name : string
      ; received : int
      }
[@@deriving show { with_path = false }]

type posting =
  { source : string
  ; destination : string
  ; asset : string
  ; amount : int64
  }
