(** This is the (deserialized) representation of a compiled numscript program *)

(** the serialized view of bytecode representing the numscript constructs (not exprs) *)
type op_source =
  | Src_Account of { account_expr_idx : int }
  | Src_AccountUnbounded of { account_expr_idx : int }
  | Src_AccountBoundedOverdraft of
      { account_expr_idx : int
      ; overdraft_expr_idx : int
      }
  | Src_Inorder of { end_idx : int }
  | Src_Max of { monetary_expr_idx : int }
  | Src_Allotment of { array_const_idx : int }
[@@deriving show { with_path = false }]

type op_dest =
  | Dest_Kept
  | Dest_Account of { account_expr_idx : int }
  | Dest_Max of { monetary_expr_idx : int }
[@@deriving show { with_path = false }]

type op_stmt =
  | Stmt_SetLocal of
      { var_uid : int
      ; typ : Common.expr_typ
      ; expr_idx : int
      }
  | Stmt_Send of
      { monetary_expr_idx : int
      ; source_idx : int
      ; destination_idx : int
      }
  | Stmt_SendAll of
      { asset_expr_idx : int
      ; source_idx : int
      ; destination_idx : int
      }
  | Stmt_Save of
      { monetary_expr_idx : int
      ; account_expr_idx : int
      }
  | Stmt_FnSetTxMeta
  | Stmt_FnSetAccountMeta
[@@deriving show { with_path = false }]

type op_expr =
  (* TODO account concat (for account literal) *)
  | Expr_FetchConst of
      { pool : [ `StringLike | `Int ]
      ; pool_idx : int
      }
  | Expr_FetchVar of
      { typ : Common.expr_typ
      ; name_idx : int
      }
  | Expr_GetLocal of
      { typ : Common.expr_typ
      ; uid : int
      }
  | Expr_NumAdd
  | Expr_NumSub
  | Expr_NumNeg
  | Expr_MkMonetary
  | Expr_MkPortion
[@@deriving show { with_path = false }]

type monetary_const =
  { asset_idx : int
  ; monetary_idx : int
  }
[@@deriving show { with_path = false }]

(* TODO double check we can share the same pool for asset, string, account *)
type constant_pool =
  { string_like : string array
  ; int : int64 array
  ; array : int array array
  }
[@@deriving show { with_path = false }]

(** The portion of bytecode representing a single expr. Basically a slice of the expr_bytecode array *)
type expr_chunk =
  { start_idx : int
  ; size : int
  }
[@@deriving show { with_path = false }]

(** the serialized repr of a compiled program.
  NOT the runtime machine *)
type t =
  { (* TODO we actually need one more indirection for the constant pool
      so that we can load the constants table during hydration
    *)
    constant_pool : constant_pool
  ; statements : op_stmt array
  ; sources : op_source array
  ; destinations : op_dest array
  ; expr_bytecode : op_expr array
  ; expr_chunks : expr_chunk array
  }
[@@deriving show { with_path = false }]
