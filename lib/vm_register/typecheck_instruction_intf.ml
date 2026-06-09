type typecheck_err =
  | TypeMismatch of
      { expected : Reg_type.t
      ; got : Reg_type.t
      ; reg : int
      }
  | UnboundReg of { reg : int }
[@@deriving show]

exception TypecheckErr of typecheck_err
