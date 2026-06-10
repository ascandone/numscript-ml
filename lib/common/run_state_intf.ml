module StringMap = Map.Make (String)

module StringPair = struct
  type t = string * string

  let compare (x1, y1) (x2, y2) =
    match String.compare x1 x2 with
    | 0 -> String.compare y1 y2
    | c -> c
  ;;
end

module PairsMap = Map.Make (StringPair)

type run_state =
  { mutable balances : (string * string, int64) Hashtbl.t
  ; sources : (string * int64) Dynarray.t
  ; postings : Common_intf.posting Dynarray.t
  ; current_asset : string ref
  }
