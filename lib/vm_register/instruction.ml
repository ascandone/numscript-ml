type reg = int

let pp_reg fmt = Format.fprintf fmt "R%d"

type t =
  | LoadConst of
      { value : [ `String of string | `Int of int64 ]
      ; dest : reg
      }
  | PullAccount of
      { cap : reg
      ; account : reg
      ; tot : reg
      }
  | JmpIfZero of
      { amount : reg
      ; delta : int
      }
  | GetAmount of
      { dest : reg
      ; src : reg
      }
  | GetAsset of
      { dest : reg
      ; src : reg
      }
  | MinMonetary of
      { dest : reg
      ; left : reg
      ; right : reg
      }
  | AddInt of
      { dest : reg
      ; left : reg
      ; right : reg
      }
  | SubInt of
      { dest : reg
      ; left : reg
      ; right : reg
      }
  | MkPortion of
      { dest : reg
      ; num : reg
      ; den : reg
      }
  | MkMonetary of
      { dest : reg
      ; asset : reg
      ; amount : reg
      }
[@@deriving show { with_path = false }]
