type value =
  [ `Int of int64
  | `String of string
  | `Portion of Portion.t
  | `Monetary of string * int64
  ]

let get_value ~reader state arg =
  let ( let* ) = Option.bind in
  let* value = Hashtbl.find_opt state arg in
  match reader value with
  | None -> failwith "unexpected type error"
  | Some value -> Some value
;;

let get_string =
  get_value ~reader:(function
    | `String x -> Some x
    | _ -> None)
;;

let get_int =
  get_value ~reader:(function
    | `Int x -> Some x
    | _ -> None)
;;

let get_portion =
  get_value ~reader:(function
    | `Portion x -> Some x
    | _ -> None)
;;

let get_monetary =
  get_value ~reader:(function
    | `Monetary x -> Some x
    | _ -> None)
;;

type state = (int, value) Hashtbl.t

let get_dest =
  let open Virtual_instruction in
  function
  | MkAllotment _ -> failwith "[TODO] mkAllot"
  | FetchVariable _ -> failwith "[TODO] fetchVar"
  | LoadConst { dest; _ }
  | UnaryOp { dest; _ }
  | BinaryOp { dest; _ }
  | PullAccount { dest; _ }
  | PullAccountUnboundedOverdraft { dest; _ } -> Some dest
  | JmpIfZero _ | Label _ | SendToAccount _ | SetCurrentAsset _ | CheckEnoughFunds _ ->
    None
;;

let eval (state : state) =
  let open Virtual_instruction in
  let ( let* ) = Option.bind in
  function
  | FetchVariable _ -> failwith "[TODO] fetchVar"
  | Label _ ->
    (* Conservative logic, we can be more precise *)
    Hashtbl.clear state;
    None
  | LoadConst { dest; value } ->
    Hashtbl.replace state dest (value :> value);
    None
  | UnaryOp { op = `get_amount; arg; dest } ->
    let* _, amt = get_monetary state arg in
    let value = `Int amt in
    Hashtbl.replace state dest value;
    Some (LoadConst { value; dest })
  | UnaryOp { op = `get_asset; arg; dest } ->
    let* asset, _ = get_monetary state arg in
    let value = `String asset in
    Hashtbl.replace state dest value;
    Some (LoadConst { value; dest })
  | UnaryOp { op = `int_copy; arg; dest } ->
    let* n = get_int state arg in
    let value = `Int n in
    Hashtbl.replace state dest value;
    Some (LoadConst { value; dest })
  | UnaryOp { op = `portion_copy; arg; dest } ->
    let* n = get_portion state arg in
    let value = `Portion n in
    Hashtbl.replace state dest value;
    None
  | BinaryOp { op = `add_int; left; right; dest } ->
    let* left = get_int state left in
    let* right = get_int state right in
    let value = `Int (Int64.add left right) in
    Hashtbl.replace state dest value;
    Some (LoadConst { value; dest })
  | BinaryOp { op = `sub_int; left; right; dest } ->
    let* left = get_int state left in
    let* right = get_int state right in
    let value = `Int (Int64.sub left right) in
    Hashtbl.replace state dest value;
    Some (LoadConst { value; dest })
  | BinaryOp { op = `sub_portion; left; right; dest } ->
    let* left = get_portion state left in
    let* right = get_portion state right in
    let value = `Portion (Portion.sub left right) in
    Hashtbl.replace state dest value;
    None
  | BinaryOp { op = `min_int; left; right; dest } ->
    let* left = get_int state left in
    let* right = get_int state right in
    let value = `Int (Int64.min left right) in
    Hashtbl.replace state dest value;
    Some (LoadConst { value; dest })
  | BinaryOp { op = `mk_monetary; left; right; dest } ->
    let* asset = get_string state left in
    let* amount = get_int state right in
    let value = `Monetary (asset, amount) in
    Hashtbl.replace state dest value;
    None
  | BinaryOp { op = `mk_portion; left; right; dest } ->
    let* num = get_int state left in
    let* den = get_int state right in
    let value = `Portion (Portion.create ~num ~den) in
    Hashtbl.replace state dest value;
    None
  (* TODO by folding the CheckEnoughFunds we could throw early instead of runtime *)
  (* TODO(perf) fold JmpIfZero *)
  (* TODO(perf) we could fold MkAllotment as well *)
  | MkAllotment _
  | PullAccount _
  | PullAccountUnboundedOverdraft _
  | JmpIfZero _
  | SendToAccount _
  | SetCurrentAsset _
  | CheckEnoughFunds _ -> None
;;

let apply instructions =
  let state = Hashtbl.create 12 in
  let updated_instr = ref false in
  let mapped_instructions =
    List.map
      (fun instr ->
         (* TODO(bug) it's not correct to compute them beforehand *)
         Option.iter (fun dest -> Hashtbl.remove state dest) (get_dest instr);
         match eval state instr with
         | None -> instr
         | Some out ->
           updated_instr := true;
           out)
      instructions
  in
  if !updated_instr then Some mapped_instructions else None
;;
