open Common

type runtime_value =
  | Value_String of string
  | Value_Int of int64
  | Value_Portion of Portion.t
  | Value_Monetary of string * int64
[@@deriving show { with_path = false }]

type exn +=
  | TypeMismatch of
      { expected : string
      ; got : runtime_value
      }

let read_int = function
  | Value_Int n -> n
  | got -> raise (TypeMismatch { expected = "int"; got })
;;

let read_string = function
  | Value_String s -> s
  | got -> raise (TypeMismatch { expected = "string"; got })
;;

let read_monetary = function
  | Value_Monetary (x, y) -> x, y
  | got -> raise (TypeMismatch { expected = "monetary"; got })
;;

let read_portion = function
  | Value_Portion x -> x
  | got -> raise (TypeMismatch { expected = "portion"; got })
;;

type t =
  { instructions : Virtual_instruction.t array
  ; labels_indexes : (string, int) Hashtbl.t
  ; pc : int ref
  ; run_state : Common.Run_state.run_state
  ; regs : runtime_value array
  }

let int64_to_non_neg = max 0L

let index_labels instructions =
  let indexes = Hashtbl.create 2 in
  Array.iteri
    (fun index instruction ->
       match instruction with
       | Virtual_instruction.Label label -> Hashtbl.replace indexes label index
       | _ -> ())
    instructions;
  indexes
;;

let create ~instructions =
  let labels_indexes = index_labels instructions in
  { instructions
  ; labels_indexes
  ; pc = ref 0
  ; run_state = Common.Run_state.create ()
  ; regs = Array.init 256 (fun _ -> Value_Int 0L)
  }
;;

exception RunError of run_error

let world_account = "world"

let alloc_var ~typ ~raw_value =
  match parse_var ~typ ~raw_value with
  | Ok (`String s) | Ok (`Asset s) | Ok (`Account s) -> Value_String s
  | Ok (`Int n) -> Value_Int n
  | Ok (`Monetary (asset, amt)) -> Value_Monetary (asset, amt)
  | Ok (`Portion p) -> Value_Portion p
  | Error `BadMonetary -> raise (RunError (BadVar { typ = `Monetary; raw_value }))
;;

let run_raise ~vars ~balances vm =
  Run_state.set_balances vm.run_state balances;
  let pc = ref 0 in
  while !pc < Array.length vm.instructions do
    let instruction = vm.instructions.(!pc) in
    incr pc;
    match instruction with
    | FetchVariable { dest; name; typ } ->
      let raw_value =
        match Run_state.StringMap.find_opt name vars with
        | None -> raise (RunError (UnboundVar name))
        | Some value -> value
      in
      vm.regs.(dest) <- alloc_var ~typ ~raw_value
    | CheckEnoughFunds { got; needed } ->
      let got = read_int vm.regs.(got) in
      let needed = read_int vm.regs.(needed) in
      if not (Int64.equal got needed) then raise (RunError MissingFunds)
    | SetCurrentAsset { asset } ->
      let asset = read_string vm.regs.(asset) in
      Run_state.set_current_asset vm.run_state asset
    | LoadConst { dest; value = `String str } -> vm.regs.(dest) <- Value_String str
    | Label _ -> ()
    | LoadConst { dest; value = `Int n } -> vm.regs.(dest) <- Value_Int n
    | MkAllotment { dest_arr; amount; portions_arr } ->
      let cap = read_int vm.regs.(amount) in
      let portions_array =
        portions_arr
        |> List.map (fun por_reg -> read_portion vm.regs.(por_reg))
        |> Array.of_list
      in
      let allots = Common.calc_allot ~portions_array ~cap |> Array.to_list in
      List.iter2
        (fun dest_reg allot_amt -> vm.regs.(dest_reg) <- Value_Int allot_amt)
        dest_arr
        allots
    | PullAccount { dest; account; overdraft; cap } ->
      let account = read_string vm.regs.(account) in
      let overdraft_bound =
        match overdraft with
        | `Bounded overdraft_reg -> read_int vm.regs.(overdraft_reg)
        | `BoundedZero -> 0L
      in
      let pulled =
        match cap with
        | None -> Run_state.pull_uncapped vm.run_state ~source:account ~overdraft_bound
        | Some cap_reg ->
          let cap = read_int vm.regs.(cap_reg) in
          Run_state.pull
            vm.run_state
            ~source:account
            ~cap
            ~overdraft_bound:
              (if String.equal world_account account then None else Some overdraft_bound)
      in
      vm.regs.(dest) <- Value_Int pulled
    | PullAccountUnboundedOverdraft { dest; account; cap } ->
      let cap = read_int vm.regs.(cap) in
      let account = read_string vm.regs.(account) in
      let pulled =
        Run_state.pull ~overdraft_bound:None vm.run_state ~source:account ~cap
      in
      vm.regs.(dest) <- Value_Int pulled
    | SendToAccount { cap = None; account } ->
      let dest = Option.map (fun account -> read_string vm.regs.(account)) account in
      Run_state.send_uncapped ?dest vm.run_state
    | SendToAccount { cap = Some cap; account } ->
      let cap = read_int vm.regs.(cap) in
      let dest = Option.map (fun account -> read_string vm.regs.(account)) account in
      Run_state.send ?dest ~cap vm.run_state
    | BinaryOp { op = `add_int; dest; left; right } ->
      let left = read_int vm.regs.(left) in
      let right = read_int vm.regs.(right) in
      vm.regs.(dest) <- Value_Int (Int64.add left right)
    | BinaryOp { op = `sub_int; dest; left; right } ->
      let left = read_int vm.regs.(left) in
      let right = read_int vm.regs.(right) in
      vm.regs.(dest) <- Value_Int (Int64.sub left right)
    | BinaryOp { op = `sub_portion; dest; left; right } ->
      let left = read_portion vm.regs.(left) in
      let right = read_portion vm.regs.(right) in
      vm.regs.(dest) <- Value_Portion (Portion.sub left right)
    | BinaryOp { op = `mk_monetary; dest; left; right } ->
      let left = read_string vm.regs.(left) in
      let right = read_int vm.regs.(right) in
      vm.regs.(dest) <- Value_Monetary (left, right)
    | BinaryOp { op = `min_int; dest; left; right } ->
      let left = read_int vm.regs.(left) in
      let right = read_int vm.regs.(right) in
      vm.regs.(dest) <- Value_Int (Int64.min left right)
    | BinaryOp { op = `mk_portion; dest; left; right } ->
      let num = read_int vm.regs.(left) in
      let den = read_int vm.regs.(right) in
      vm.regs.(dest) <- Value_Portion (Portion.create ~num ~den)
    | UnaryOp { op = `int_copy; dest; arg } ->
      let n = read_int vm.regs.(arg) in
      vm.regs.(dest) <- Value_Int n
    | UnaryOp { op = `portion_copy; dest; arg } ->
      let n = read_portion vm.regs.(arg) in
      vm.regs.(dest) <- Value_Portion n
    | UnaryOp { op = `get_amount; dest; arg } ->
      let _, amount = read_monetary vm.regs.(arg) in
      vm.regs.(dest) <- Value_Int amount
    | UnaryOp { op = `get_asset; dest; arg } ->
      let asset, _ = read_monetary vm.regs.(arg) in
      vm.regs.(dest) <- Value_String asset
    | JmpIfZero { value; label } ->
      let value = read_int vm.regs.(value) in
      if Int64.equal Int64.zero value then pc := Hashtbl.find vm.labels_indexes label
  done;
  Run_state.get_postings vm.run_state
;;

let run ~vars ~balances vm =
  try Ok (run_raise ~vars ~balances vm) with
  | RunError e -> Error e
;;
