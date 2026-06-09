open Common

type registers =
  { ints : int64 array
  ; strings : string array
  ; portions : Portion.t array
  ; monetaries : (string * int64) array
  }

type t =
  { instructions : Instruction.t array
  ; pc : int ref
  ; run_state : Common.Run_state.run_state
  ; regs : registers
  }

let run vm =
  let pc = ref 0 in
  while !pc < Array.length vm.instructions do
    let instruction = vm.instructions.(!pc) in
    incr pc;
    match instruction with
    | LoadConst { dest; value = `String str } -> vm.regs.strings.(dest) <- str
    | LoadConst { dest; value = `Int n } -> vm.regs.ints.(dest) <- n
    | PullAccount { cap; account; tot } ->
      let cap = vm.regs.ints.(cap) in
      let account = vm.regs.strings.(account) in
      let pulled = Run_state.pull vm.run_state ~source:account ~cap in
      vm.regs.ints.(tot) <- Int64.add vm.regs.ints.(tot) pulled
    | AddInt { dest; left; right } ->
      let left = vm.regs.ints.(left) in
      let right = vm.regs.ints.(right) in
      vm.regs.ints.(dest) <- Int64.add left right
    | SubInt { dest; left; right } ->
      let left = vm.regs.ints.(left) in
      let right = vm.regs.ints.(right) in
      vm.regs.ints.(dest) <- Int64.sub left right
    | GetAmount { dest; src } ->
      let _, amount = vm.regs.monetaries.(src) in
      vm.regs.ints.(dest) <- amount
    | GetAsset { dest; src } ->
      let asset, _ = vm.regs.monetaries.(src) in
      vm.regs.strings.(dest) <- asset
    | MinMonetary { dest; left; right } ->
      let asset1, left = vm.regs.monetaries.(left) in
      let asset2, right = vm.regs.monetaries.(right) in
      assert (asset1 = asset2);
      vm.regs.monetaries.(dest) <- asset1, Int64.min left right
    | MkPortion { dest; num; den } ->
      let num = vm.regs.ints.(num) in
      let den = vm.regs.ints.(den) in
      vm.regs.portions.(dest) <- Portion.create ~num ~den
    | MkMonetary { dest; asset; amount } ->
      let asset = vm.regs.strings.(asset) in
      let amount = vm.regs.ints.(amount) in
      vm.regs.monetaries.(dest) <- asset, amount
    | JmpIfZero { amount; delta } ->
      let amount = vm.regs.ints.(amount) in
      if Int64.equal Int64.zero amount then pc := !pc + delta
  done;
  vm.run_state.postings |> Queue.to_seq |> List.of_seq
;;
