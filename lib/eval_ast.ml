type 'asset ctx = { vars : (string, Value.t) Hashtbl.t }

let rec eval_expr ctx = function
  | Ast.ExprVar name -> Hashtbl.find ctx.vars name
  | Ast.ExprInt n -> Value.Int n
  | Ast.ExprPerc _ -> failwith "TODO perc"
  | Ast.ExprMonetaryLit (mon, amt) ->
    let mon_val = Value.expect (eval_expr ctx mon) Value.expect_asset in
    let amt_val = Value.expect (eval_expr ctx amt) Value.expect_number in
    Monetary (mon_val, amt_val)
  | Ast.ExprInfix (Ast.Add, left, right) ->
    let left_val = Value.expect (eval_expr ctx left) Value.expect_number in
    let right_val = Value.expect (eval_expr ctx right) Value.expect_number in
    Int (left_val + right_val)
  | Ast.ExprInfix (Ast.Sub, left, right) ->
    let left_val = Value.expect (eval_expr ctx left) Value.expect_number in
    let right_val = Value.expect (eval_expr ctx right) Value.expect_number in
    Int (left_val - right_val)
  | Ast.ExprInfix (Ast.Div, left, right) ->
    let left_val = Value.expect (eval_expr ctx left) Value.expect_number in
    let right_val = Value.expect (eval_expr ctx right) Value.expect_number in
    Portion (left_val, right_val)
;;

let rec eval_source ctx = function
  | Ast.SrcAccount acc ->
    let acc_val = Value.expect (eval_expr ctx acc) Value.expect_asset in
    Ast_canonical.SrcAccount acc_val
  | Ast.SrcMax (cap, src) ->
    (* TODO check asset *)
    let _asset, cap_val = Value.expect (eval_expr ctx cap) Value.expect_monetary in
    Ast_canonical.SrcMax (cap_val, eval_source ctx src)
  | Ast.SrcInorder srcs -> Ast_canonical.SrcInorder (List.map (eval_source ctx) srcs)
  | Ast.SrcAllotment allots ->
    Ast_canonical.SrcAllotment
      (List.map
         (fun (por, src) ->
            let num, den = Value.expect (eval_expr ctx por) Value.expect_portion in
            let ev_src = eval_source ctx src in
            Ast_canonical.Portion (num, den), ev_src)
         allots)
;;

let rec eval_dest ctx = function
  | Ast.DestAccount acc ->
    let acc_val = Value.expect (eval_expr ctx acc) Value.expect_asset in
    Ast_canonical.DestAccount acc_val
  | Ast.DestInorder (dests, rem) ->
    Ast_canonical.DestInorder
      ( List.map
          (fun (clause : Ast.dest_inorder_clause) : Ast_canonical.dest_inorder_clause ->
             { cap = 0; dest = eval_kept_or_dest ctx clause.dest })
          dests
      , eval_kept_or_dest ctx rem )
  | Ast.DestAllotment _ -> failwith "TODO SrcAllotment"

and eval_kept_or_dest ctx = function
  | Ast.Kept -> Ast_canonical.Kept
  | Ast.Dest dest -> Ast_canonical.Dest (eval_dest ctx dest)
;;

let eval_statement ctx = function
  | Ast.StmtSendAll { asset; source; destination } ->
    let asset = Value.expect (eval_expr ctx asset) Value.expect_asset in
    let source = eval_source ctx source in
    let destination = eval_dest ctx destination in
    Ast_canonical.StmtSendAll { asset; source; destination }
  | Ast.StmtSend { monetary; source; destination } ->
    let asset, amount = Value.expect (eval_expr ctx monetary) Value.expect_monetary in
    let source = eval_source ctx source in
    let destination = eval_dest ctx destination in
    Ast_canonical.StmtSend { asset; amount; source; destination }
  | Ast.Save { monetary; account } ->
    let asset, amount = Value.expect (eval_expr ctx monetary) Value.expect_monetary in
    let account = Value.expect (eval_expr ctx account) Value.expect_account in
    Ast_canonical.Save { asset; amount; account }
;;
