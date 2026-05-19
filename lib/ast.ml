type binop =
  | Add
  | Sub
  | Div

type expr =
  | ExprVar of string
  | ExprAccount of string
  | ExprString of string
  | ExprAsset of string
  | ExprPerc of int
  | ExprInt of int
  | ExprInfix of binop * expr * expr
  | ExprMonetaryLit of expr * expr
  | ExprFnCall of string * expr list

type source =
  | SrcAccount of expr
  | SrcAccountOverdraft of
      { account : expr
      ; max_overdraft : expr option
      }
  | SrcMax of expr * source
  | SrcInorder of source list
  | SrcAllotment of (expr option * source) list

type dest =
  | DestAccount of expr
  | DestInorder of dest_inorder_clause list * kept_or_dest
  | DestAllotment of (expr option * kept_or_dest) list

and kept_or_dest =
  | Kept
  | Dest of dest

and dest_inorder_clause =
  { cap : expr
  ; dest : kept_or_dest
  }

type statement =
  | StmtSend of
      { monetary : expr
      ; source : source
      ; destination : dest
      }
  | StmtSendAll of
      { asset : expr
      ; source : source
      ; destination : dest
      }
  | Save of
      { monetary : expr
      ; account : expr
      }

type var =
  { typ : string
  ; name : string
  ; value : expr option
  }

type program =
  { vars : var list
  ; statements : statement list
  }
