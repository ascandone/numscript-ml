module StringPair = struct
  type t = string * string

  let compare (x1, y1) (x2, y2) =
    match String.compare x1 x2 with
    | 0 -> String.compare y1 y2
    | c -> c
  ;;
end

module PairsMap = Map.Make (StringPair)

type 't pairs_map = 't PairsMap.t

module StringMap = Map.Make (String)

type posting =
  { source : string
  ; destination : string
  ; asset : string
  ; amount : int64
  }
