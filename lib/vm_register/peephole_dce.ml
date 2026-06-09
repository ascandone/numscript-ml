let overdraft_to_reg_list = function
  | `BoundedZero | `Unbounded -> []
  | `Bounded reg -> [ reg ]
;;

let get_used_args =
  let open Virtual_instruction in
  function
  | LoadConst _ | Label _ -> []
  | BinaryOp { left; right } -> [ left; right ]
  | UnaryOp { arg; _ } -> [ arg ]
  | JmpIfZero { value } -> [ value ]
  | CheckEnoughFunds { got; needed } -> [ got; needed ]
  | SendToAccount { account; cap } -> account :: Option.to_list cap
  | PullAccountUnboundedOverdraft { account; cap } -> [ account; cap ]
  | PullAccount { account; cap; overdraft } ->
    account :: List.concat [ Option.to_list cap; overdraft_to_reg_list overdraft ]
  | SetCurrentAsset { asset } -> [ asset ]
;;

let is_instruction_useful used_regs =
  let open Virtual_instruction in
  function
  | Label _
  | SendToAccount _
  | JmpIfZero _
  | CheckEnoughFunds _
  | PullAccount _
  | PullAccountUnboundedOverdraft _
  | SetCurrentAsset _ -> true
  | LoadConst { dest } | BinaryOp { dest } | UnaryOp { dest } ->
    Hashtbl.mem used_regs dest
;;

let apply code =
  let used_regs_set =
    code
    |> List.to_seq
    |> Seq.map get_used_args
    |> Seq.concat_map List.to_seq
    |> Seq.map (fun v -> v, ())
    |> Hashtbl.of_seq
  in
  let changed_list = ref false in
  let new_lst =
    code
    |> List.filter (fun instr ->
      let is_useful = is_instruction_useful used_regs_set instr in
      if not is_useful then changed_list := true;
      is_useful)
  in
  if !changed_list then Some new_lst else None
;;
