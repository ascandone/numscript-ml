include Compiler_intf
open Syntax

type ctx =
  { instructions : Instruction.t Dynarray.t
  ; next_string_reg : int ref
  ; next_int_reg : int ref
  ; next_portion_reg : int ref
  ; next_monetary_reg : int ref
  }

let get_fresh_dest reg_ref =
  let value = !reg_ref in
  incr reg_ref;
  value
;;

let push_instruction ctx instr = Dynarray.add_last ctx.instructions instr

let rec compile_expr_to ~dest ctx (expr : Ast.expr) =
  match expr with
  | Ast.ExprVar _ -> failwith "[TODO] var"
  | Ast.ExprAccount str | Ast.ExprAsset str | Ast.ExprString str ->
    push_instruction ctx @@ LoadConst { value = `String str; dest }
  | Ast.ExprInt n ->
    push_instruction ctx @@ LoadConst { value = `Int (Int64.of_int n); dest }
  | Ast.ExprMonetaryLit (asset, amount) ->
    let asset = compile_expr ctx asset ~reg_pool:ctx.next_string_reg in
    let amount = compile_expr ctx amount ~reg_pool:ctx.next_string_reg in
    push_instruction ctx @@ MkMonetary { amount; asset; dest }
  | Ast.ExprInfix (Ast.Add, left, right) ->
    let left = compile_expr ctx left ~reg_pool:ctx.next_int_reg in
    let right = compile_expr ctx right ~reg_pool:ctx.next_int_reg in
    push_instruction ctx @@ AddInt { left; right; dest }
  | Ast.ExprInfix (Ast.Sub, left, right) ->
    let left = compile_expr ctx left ~reg_pool:ctx.next_int_reg in
    let right = compile_expr ctx right ~reg_pool:ctx.next_int_reg in
    push_instruction ctx @@ SubInt { left; right; dest }
  | Ast.ExprInfix (Ast.Div, num, den) ->
    let num = compile_expr ctx num ~reg_pool:ctx.next_int_reg in
    let den = compile_expr ctx den ~reg_pool:ctx.next_int_reg in
    push_instruction ctx @@ MkPortion { num; den; dest }
  | Ast.ExprPerc _ -> failwith "[TODO] perc"
  | Ast.ExprFnCall _ -> failwith "[TODO] fn call"

and compile_expr ~reg_pool ctx expr =
  let dest = get_fresh_dest reg_pool in
  compile_expr_to ctx ~dest expr;
  dest
;;

let rec compile_source ~tot_reg ~cap_reg ctx (source : Ast.source) =
  match source, cap_reg with
  | Ast.SrcAccountOverdraft { max_overdraft = None; _ }, None ->
    failwith "compilation error: uncapped overdraft"
  | Ast.SrcAccountOverdraft _, _ -> failwith "[TODO] overdraft"
  | Ast.SrcAccount name, None ->
    let _account_reg = compile_expr ctx name ~reg_pool:ctx.next_monetary_reg in
    failwith "[TODO] uncapped pull"
  | Ast.SrcAccount name, Some cap ->
    let account_reg = compile_expr ctx name ~reg_pool:ctx.next_monetary_reg in
    push_instruction ctx (PullAccount { cap; account = account_reg; tot = tot_reg })
  | Ast.SrcMax (cap, sub_src), None ->
    let cap_reg = compile_expr ctx cap ~reg_pool:ctx.next_monetary_reg in
    (* TODO should we extract the amount and pass that instead of monetary?  *)
    compile_source ctx ~tot_reg ~cap_reg:(Some cap_reg) sub_src
  | Ast.SrcMax (max_cap, sub_src), Some outer_cap_reg ->
    let max_cap_reg = compile_expr ctx max_cap ~reg_pool:ctx.next_monetary_reg in
    let actual_cap_reg = get_fresh_dest ctx.next_monetary_reg in
    push_instruction
      ctx
      (MinMonetary { dest = actual_cap_reg; left = max_cap_reg; right = outer_cap_reg });
    (* TODO should we create a new tot_reg? probably not *)
    compile_source ~tot_reg ~cap_reg:(Some actual_cap_reg) ctx sub_src
  | Ast.SrcInorder _, None -> failwith "compilation err: uncapped inorder"
  | Ast.SrcInorder [], _ -> failwith "compilation err: empty inorder"
  | Ast.SrcInorder [ sub_src ], Some outer_cap_reg ->
    compile_source ctx ~tot_reg ~cap_reg:(Some outer_cap_reg) sub_src
  | Ast.SrcInorder (sub_src :: sub_srcs), Some outer_cap_reg ->
    compile_source ctx ~tot_reg ~cap_reg:(Some outer_cap_reg) sub_src;
    let instruction_count_before = Dynarray.length ctx.instructions in
    (*  we emit a dummy instruction now, and schedule a patch with the correct delta *)
    push_instruction ctx (JmpIfZero { amount = 0; delta = 0 });
    compile_source ~tot_reg ~cap_reg ctx (Ast.SrcInorder sub_srcs);
    (* we patch the previous delta: *)
    let instruction_count_after = Dynarray.length ctx.instructions in
    Dynarray.set
      ctx.instructions
      instruction_count_before
      (JmpIfZero
         { amount = tot_reg; delta = instruction_count_after - instruction_count_before })
  | Ast.SrcAllotment _, None -> failwith "compilation err: uncapped allotment"
  | Ast.SrcAllotment _, Some _ -> failwith "TODO"
;;

let compile_parsed syntax =
  let ctx : ctx =
    { instructions = Dynarray.create ()
    ; next_string_reg = ref 0
    ; next_int_reg = ref 0
    ; next_portion_reg = ref 0
    ; next_monetary_reg = ref 0
    }
  in
  let compiled : compiled_program =
    { instructions = Dynarray.to_array ctx.instructions }
  in
  Ok compiled
;;
