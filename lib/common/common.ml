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

let calc_allot ~portions_array ~cap =
  let values_to_send_first_pass =
    Array.map
      (fun por ->
         let num = Portion.num por in
         let den = Portion.den por in
         let floored_down = Int64.div (Int64.mul cap num) den in
         floored_down)
      portions_array
  in
  let total_sent_first_pass = Array.fold_left Int64.add 0L values_to_send_first_pass in
  let remainder_ref = ref (Int64.sub cap total_sent_first_pass) in
  Array.map
    (fun needed_amt ->
       if !remainder_ref > 0L
       then (
         remainder_ref := Int64.sub !remainder_ref 1L;
         Int64.add 1L needed_amt)
       else needed_amt)
    values_to_send_first_pass
;;
