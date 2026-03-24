module StringMap = Map.Make (String)

type 'a string_map = 'a StringMap.t

let string_map_of_yojson value_of_yojson = function
  | `Assoc pairs ->
    let rec aux map = function
      | [] -> Ok map
      | (k, v) :: rest ->
        (match value_of_yojson v with
         | Ok v_parsed -> aux (StringMap.add k v_parsed map) rest
         | Error e -> Error ("Error parsing key '" ^ k ^ "': " ^ e))
    in
    aux StringMap.empty pairs
  | _ -> Error "Expected a JSON object for map"
;;

(* Convert Immutable Map -> JSON object *)
let string_map_to_yojson value_to_yojson map =
  let pairs = StringMap.bindings map |> List.map (fun (k, v) -> k, value_to_yojson v) in
  `Assoc pairs
;;

type balances = float string_map string_map [@@deriving yojson]
type variables_map = string string_map [@@deriving yojson]
type accounts_metadata = string string_map string_map [@@deriving yojson]
type tx_metadata = string string_map [@@deriving yojson]

(* The root Inputs object *)
type inputs =
  { schema : string option [@key "$schema"] [@default None]
  ; balances : balances option [@default None]
  ; variables : variables_map option [@default None]
  ; metadata : accounts_metadata option [@default None]
  ; feature_flags : string list option [@key "featureFlags"] [@default None]
  }
[@@deriving yojson]
