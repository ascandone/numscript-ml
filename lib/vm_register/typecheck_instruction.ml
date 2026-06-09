include Typecheck_instruction_intf

type typecheck_state = (int, Reg_type.t) Hashtbl.t

let create_state () = Hashtbl.create 10

let check_reg_ state reg expected =
  match Hashtbl.find_opt state reg with
  | None -> raise (TypecheckErr (UnboundReg { reg }))
  | Some got when not (Reg_type.eq got expected) ->
    raise (TypecheckErr (TypeMismatch { expected; got; reg }))
  | Some _ -> ()
;;

let declare_reg_ state reg expected =
  match Hashtbl.find_opt state reg with
  | Some got when not (Reg_type.eq got expected) ->
    raise (TypecheckErr (TypeMismatch { expected; got; reg }))
  | Some _ -> ()
  | None -> Hashtbl.replace state reg expected
;;

let push_instruction state =
  let check_reg = check_reg_ state in
  let declare_reg = declare_reg_ state in
  function
  | Virtual_instruction.LoadConst { value; dest } ->
    let typ =
      match value with
      | `String _ -> Reg_type.string
      | `Int _ -> Reg_type.int
    in
    declare_reg dest typ
  | Virtual_instruction.MkAllotment
      { dest_start; amount; portions_arr = portions_start, len } ->
    check_reg amount Reg_type.int;
    for por = portions_start to portions_start + len - 1 do
      check_reg por Reg_type.portion
    done;
    for dest = dest_start to dest_start + len - 1 do
      declare_reg dest Reg_type.int
    done
  | Virtual_instruction.SetCurrentAsset { asset } -> check_reg asset Reg_type.string
  | Virtual_instruction.CheckEnoughFunds { got; needed } ->
    check_reg got Reg_type.int;
    check_reg needed Reg_type.int
  | Virtual_instruction.PullAccount { dest; account; cap; overdraft } ->
    check_reg account Reg_type.string;
    Option.iter (fun cap -> check_reg cap Reg_type.int) cap;
    (match overdraft with
     | `Bounded reg -> check_reg reg Reg_type.int
     | `BoundedZero -> ());
    declare_reg dest Reg_type.int
  | Virtual_instruction.PullAccountUnboundedOverdraft { dest; account; cap } ->
    check_reg account Reg_type.string;
    check_reg cap Reg_type.int;
    declare_reg dest Reg_type.int
  | Virtual_instruction.SendToAccount { account; cap } ->
    check_reg account Reg_type.string;
    Option.iter (fun cap -> check_reg cap Reg_type.int) cap
  | Virtual_instruction.Label _ ->
    (* TODO check label only jump forward *)
    ()
  | Virtual_instruction.JmpIfZero { value; label = _ } -> check_reg value Reg_type.int
  | Virtual_instruction.BinaryOp { op; left; right; dest } ->
    let left_typ, right_typ, dest_typ =
      let open Reg_type in
      match op with
      | `add_int | `sub_int | `min_int -> int, int, int
      | `mk_monetary -> string, int, monetary
      | `mk_portion -> int, int, portion
    in
    check_reg left left_typ;
    check_reg right right_typ;
    declare_reg dest dest_typ
  | Virtual_instruction.UnaryOp { op; arg; dest } ->
    let arg_typ, dest_typ =
      let open Reg_type in
      match op with
      | `get_amount -> monetary, int
      | `get_asset -> monetary, string
      | `int_copy -> int, int
    in
    check_reg arg arg_typ;
    declare_reg dest dest_typ
;;
