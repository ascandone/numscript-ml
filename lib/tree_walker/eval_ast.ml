open Common

type 'asset ctx =
  { vars : (string, Value.t) Hashtbl.t
  ; state : Run_state.run_state
  }

let rec eval_expr ctx =
  let open Syntax in
  function
  | Ast.ExprVar name ->
    (match Hashtbl.find_opt ctx.vars name with
     | None -> failwith (Format.sprintf "Error: var `%s` not found" name)
     | Some v -> v)
  | Ast.ExprAccount name -> Value.Asset name
  | Ast.ExprAsset name -> Value.Asset name
  | Ast.ExprString s -> Value.String s
  | Ast.ExprInt n -> Value.Int (Int64.of_int n)
  | Ast.ExprPerc n -> Value.Portion (Portion.create ~num:(Int64.of_int n) ~den:100L)
  | Ast.ExprMonetaryLit (mon, amt) ->
    let mon_val = Value.expect (eval_expr ctx mon) Value.expect_asset in
    let amt_val = Value.expect (eval_expr ctx amt) Value.expect_number in
    Monetary (mon_val, amt_val)
  | Ast.ExprInfix (Ast.Add, left, right) ->
    let left_val = Value.expect (eval_expr ctx left) Value.expect_number in
    let right_val = Value.expect (eval_expr ctx right) Value.expect_number in
    Int (Int64.add left_val right_val)
  | Ast.ExprInfix (Ast.Sub, left, right) ->
    let left_val = Value.expect (eval_expr ctx left) Value.expect_number in
    let right_val = Value.expect (eval_expr ctx right) Value.expect_number in
    Int (Int64.add left_val right_val)
  | Ast.ExprInfix (Ast.Div, left, right) ->
    let num = Value.expect (eval_expr ctx left) Value.expect_number in
    let den = Value.expect (eval_expr ctx right) Value.expect_number in
    Portion (Portion.create ~num ~den)
  | Ast.ExprFnCall ("balance", [ acc_expr; asset_expr ]) ->
    let account = Value.expect (eval_expr ctx acc_expr) Value.expect_asset in
    let asset = Value.expect (eval_expr ctx asset_expr) Value.expect_asset in
    Value.Monetary (asset, Run_state.get_account_balance ctx.state ~account ~asset)
  | Ast.ExprFnCall (name, _) -> failwith (Format.sprintf "Unknown function: %s" name)
;;

let eval_overdraft_bound ctx expr =
  match eval_expr ctx expr with
  | Value.Monetary (_, amt) -> amt
  | _ -> failwith "overdraft bound must be a monetary or number"
;;

let rec eval_source ctx =
  let open Syntax in
  function
  | Ast.SrcAccount acc ->
    let acc_val = Value.expect (eval_expr ctx acc) Value.expect_asset in
    if acc_val = "world"
    then Ast_canonical.SrcAccountOverdraft { account = "world"; max_overdraft = None }
    else Ast_canonical.SrcAccount acc_val
  | Ast.SrcAccountOverdraft { account; max_overdraft } ->
    let acc_val = Value.expect (eval_expr ctx account) Value.expect_asset in
    let max_overdraft =
      max_overdraft
      |> Option.map (fun expr -> Value.expect (eval_expr ctx expr) Value.expect_monetary)
    in
    Ast_canonical.SrcAccountOverdraft { account = acc_val; max_overdraft }
  | Ast.SrcMax (cap, src) ->
    (* TODO check asset *)
    let _asset, cap_val = Value.expect (eval_expr ctx cap) Value.expect_monetary in
    Ast_canonical.SrcMax (cap_val, eval_source ctx src)
  | Ast.SrcInorder srcs -> Ast_canonical.SrcInorder (List.map (eval_source ctx) srcs)
  | Ast.SrcAllotment _ -> failwith "[TODO] eval allot"
;;

let rec eval_dest ctx =
  let open Syntax in
  function
  | Ast.DestAccount acc ->
    let acc_val = Value.expect (eval_expr ctx acc) Value.expect_asset in
    Ast_canonical.DestAccount acc_val
  | Ast.DestInorder (dests, rem) ->
    Ast_canonical.DestInorder
      ( List.map
          (fun (clause : Ast.dest_inorder_clause) : Ast_canonical.dest_inorder_clause ->
             let cap = eval_overdraft_bound ctx clause.cap in
             { cap; dest = eval_kept_or_dest ctx clause.dest })
          dests
      , eval_kept_or_dest ctx rem )
  | Ast.DestAllotment allots -> failwith "[TODO] eval allot"

and eval_kept_or_dest ctx =
  let open Syntax in
  function
  | Ast.Kept -> Ast_canonical.Kept
  | Ast.Dest dest -> Ast_canonical.Dest (eval_dest ctx dest)
;;

let eval_statement ctx =
  let open Syntax in
  function
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
  | Ast.FnStatement _ -> failwith "TODO fn"
;;
