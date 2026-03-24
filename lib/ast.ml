type binop =
  | Add
  | Sub
  | Div

type expr =
  | ExprVar of string
  | ExprPerc of int
  | ExprInt of int
  | ExprInfix of binop * expr * expr
  | ExprMonetaryLit of expr * expr

type source =
  | SrcAccount of expr
  | SrcMax of expr * source
  | SrcInorder of source list
  | SrcAllotment of (expr * source) list

type dest =
  | DestAccount of expr
  | DestInorder of dest_inorder_clause list * kept_or_dest
  | DestAllotment of (expr * dest) list

and kept_or_dest =
  | Kept
  | Dest of dest

and dest_inorder_clause =
  { cap : expr
  ; dest : kept_or_dest
  }
