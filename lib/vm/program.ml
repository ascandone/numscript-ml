(** This is the (deserialized) representation of a compiled numscript program *)

(** the serialized view of bytecode representing the numscript constructs (not exprs) *)
type op_source =
  | Src_Account of { account_idx : int }
  | Src_Inorder of { end_idx : int }
  | Src_Max of { monetary_idx : int }

type op_dest = Dest_Account of { account_idx : int }

type op_stmt =
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

type expr_typ =
  | ExprTyp_Number
  | ExprTyp_String
  | ExprTyp_Account
  | ExprTyp_Asset
(* | ExprTyp_Monetary
  | ExprTyp_Portion *)

type op_expr =
  (* TODO account concat (for account literal) *)
  | Expr_FetchConst of
      { typ : expr_typ
      ; pool_idx : int
      }
  | Expr_NumAdd
  | Expr_NumSub
  | Expr_NumNeg
  | Expr_MkMonetary
  | Expr_MkPortion

type monetary_const =
  { asset_idx : int
  ; monetary_idx : int
  }

(* TODO double check we can share the same pool for asset, string, account *)
type constant_pool =
  { string_like : string array
  ; int : int64 array
  }

(** The portion of bytecode representing a single expr. Basically a slice of the expr_bytecode array *)
type expr_chunk =
  { start_idx : int
  ; size : int
  }

(** the serialized repr of a compiled program.
  NOT the runtime machine *)
type t =
  { constant_pool : constant_pool
  ; statements : op_stmt array
  ; sources : op_source array
  ; destinations : op_dest array
  ; expr_bytecode : op_expr array
  ; expr_chunks : expr_chunk array
  }
