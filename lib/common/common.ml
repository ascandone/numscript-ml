include Common_intf
module Run_state = Run_state

let parse_typ typ_name =
  match typ_name with
  | "account" -> Some ExprTyp_Account
  | "asset" -> Some ExprTyp_Asset
  | "string" -> Some ExprTyp_String
  | "number" -> Some ExprTyp_Number
  | "portion" -> Some ExprTyp_Portion
  | "monetary" -> Some ExprTyp_Monetary
  | _ -> None
;;
