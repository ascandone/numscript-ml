module StringMap = Map.Make (String)

type 'a string_map = 'a StringMap.t

(* Convert JSON object -> Immutable Map *)
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

(* --- 2. Schema Definitions --- *)

(* The deriver will automatically use string_map_of_yojson for these! *)
type balances = float string_map string_map [@@deriving yojson]
type variables_map = string string_map [@@deriving yojson]
type accounts_metadata = string string_map string_map [@@deriving yojson]
type tx_metadata = string string_map [@@deriving yojson]

(* Movements maps source -> destination -> asset -> amount *)
type movements = float string_map string_map string_map [@@deriving yojson]

type posting =
  { source : string
  ; destination : string
  ; asset : string
  ; amount : float
  }
[@@deriving yojson]

type test_case =
  { it : string
  ; focus : bool option [@default None]
  ; skip : bool option [@default None]
  ; balances : balances option [@default None]
  ; variables : variables_map option [@default None]
  ; metadata : accounts_metadata option [@default None]
  ; expect_postings : posting list option [@key "expect.postings"] [@default None]
  ; expect_end_balances : balances option [@key "expect.endBalances"] [@default None]
  ; expect_end_balances_include : balances option
        [@key "expect.endBalances.include"] [@default None]
  ; expect_movements : movements option [@key "expect.movements"] [@default None]
  ; expect_tx_metadata : tx_metadata option [@key "expect.txMetadata"] [@default None]
  ; expect_metadata : accounts_metadata option [@key "expect.metadata"] [@default None]
  ; expect_error_missing_funds : bool option
        [@key "expect.error.missingFunds"] [@default None]
  }
[@@deriving yojson]

type specs =
  { schema : string option [@key "$schema"] [@default None]
  ; balances : balances option [@default None]
  ; variables : variables_map option [@default None]
  ; metadata : accounts_metadata option [@default None]
  ; test_cases : test_case list [@key "testCases"]
  ; feature_flags : string list option [@key "featureFlags"] [@default None]
  }
[@@deriving yojson]

let parse_source json_string =
  let raw_json = Yojson.Safe.from_string json_string in
  specs_of_yojson raw_json
;;

let parse_file path =
  let raw_json = Yojson.Safe.from_file path in
  specs_of_yojson raw_json
;;
