type run_state =
  { balances : (string * string, int64) Hashtbl.t
  ; sources : (string * int64) Dynarray.t
  ; postings : Common_intf.posting Queue.t
  ; current_asset : string ref
  }
