type reg = int

let pp_reg fmt = Format.fprintf fmt "$r%d"

let pp_reg_arr fmt (reg, len) =
  Format.fprintf fmt "%a..%a" pp_reg reg pp_reg (reg + len - 1)
;;

type unary_op =
  [ `get_amount
  | `get_asset
  | `int_copy
  ]
[@@deriving show { with_path = false }]

type binary_op =
  [ `min_int
  | `add_int
  | `sub_int
  | `mk_portion
  | `mk_monetary
  ]
[@@deriving show { with_path = false }]

type const_value =
  [ `String of string
  | `Int of int64
  ]

let pp_const_value fmt = function
  | `String s -> Format.fprintf fmt "\"%s\"" s
  | `Int n -> Format.fprintf fmt "%Ld" n
;;

type t =
  | SetCurrentAsset of { asset : reg }
  | CheckEnoughFunds of
      { got : reg
      ; needed : reg
      }
  | PullAccount of
      { dest : reg
      ; account : reg
      ; cap : reg option
      ; overdraft : [ `BoundedZero | `Bounded of reg ]
      }
  | PullAccountUnboundedOverdraft of
      { dest : reg
      ; account : reg
      ; cap : reg
      }
  | MkAllotment of
      { dest_start : reg
      ; amount : reg
      ; portions_arr : reg * int
      }
  | SendToAccount of
      { account : reg
      ; cap : reg option
      }
  | LoadConst of
      { value : [ `String of string | `Int of int64 ]
      ; dest : reg
      }
  | Label of string
  | JmpIfZero of
      { value : reg
      ; label : string
      }
  | UnaryOp of
      { op : unary_op
      ; dest : reg
      ; arg : reg
      }
  | BinaryOp of
      { op : binary_op
      ; dest : reg
      ; left : reg
      ; right : reg
      }

let pp fmt = function
  | CheckEnoughFunds { got; needed } ->
    Format.fprintf fmt "check_enough_funds(%a, %a)" pp_reg got pp_reg needed
  | SetCurrentAsset { asset } -> Format.fprintf fmt "set_current_asset(%a)" pp_reg asset
  | LoadConst { dest; value } ->
    Format.fprintf fmt "%a <- load_const(%a)" pp_reg dest pp_const_value value
  | SendToAccount { account; cap = None } ->
    Format.fprintf fmt "send_to_account_uncapped(%a)" pp_reg account
  | SendToAccount { account; cap = Some cap } ->
    Format.fprintf fmt "send_to_account_capped(%a, %a)" pp_reg account pp_reg cap
  | PullAccount { dest; account; cap; overdraft } ->
    let cap_label =
      match cap with
      | None -> ""
      | Some cap -> Format.asprintf ", cap: %a" pp_reg cap
    in
    let overdraft_label =
      match overdraft with
      | `BoundedZero -> ""
      | `Bounded reg -> Format.asprintf ", overdraft: %a" pp_reg reg
    in
    Format.fprintf
      fmt
      "%a <- pull_account(account: %a%s%s)"
      pp_reg
      dest
      pp_reg
      account
      cap_label
      overdraft_label
  | PullAccountUnboundedOverdraft { dest; account; cap } ->
    Format.fprintf
      fmt
      "%a <- pull_account_unbounded_overdraft(account: %a, cap: %a)"
      pp_reg
      dest
      pp_reg
      account
      pp_reg
      cap
  | MkAllotment { dest_start; amount; portions_arr = portions_start, len } ->
    Format.fprintf
      fmt
      "%a <- mk_allot(%a, %a)"
      pp_reg_arr
      (dest_start, len)
      pp_reg
      amount
      pp_reg_arr
      (portions_start, len)
  | Label label -> Format.fprintf fmt "#%s" label
  | JmpIfZero { value; label } ->
    Format.fprintf fmt "jmp_if_zero(%a, #%s)" pp_reg value label
  | UnaryOp { dest; op; arg } ->
    let op_name = show_unary_op op in
    let op_name = String.sub op_name 1 (String.length op_name - 1) in
    Format.fprintf fmt "%a <- %s(%a)" pp_reg dest op_name pp_reg arg
  | BinaryOp { dest; op; left; right } ->
    let op_name = show_binary_op op in
    let op_name = String.sub op_name 1 (String.length op_name - 1) in
    Format.fprintf fmt "%a <- %s(%a, %a)" pp_reg dest op_name pp_reg left pp_reg right
;;

let pp_program fmt =
  Array.iter (fun i ->
    pp fmt i;
    Format.fprintf fmt "\n")
;;
