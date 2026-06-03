module StringPair = struct
  type t = string * string

  let compare (x1, y1) (x2, y2) =
    match String.compare x1 x2 with
    | 0 -> String.compare y1 y2
    | c -> c
  ;;
end

module PairsMap = Map.Make (StringPair)
module StringMap = Map.Make (String)

(* Assuming PairsMap is already defined with String * String key *)
type 't pairs_map = 't PairsMap.t

let pairs_map_of_yojson value_of_yojson = function
  | `Assoc outer_pairs ->
    let open Result in
    let ( >>= ) = Result.bind in
    (* Iterate through the first level of the JSON object *)
    List.fold_left
      (fun acc_res (k_outer, outer_val) ->
         acc_res
         >>= fun acc ->
         match outer_val with
         | `Assoc inner_pairs ->
           (* Iterate through the second level *)
           List.fold_left
             (fun inner_acc_res (k_inner, v_json) ->
                inner_acc_res
                >>= fun inner_acc ->
                value_of_yojson v_json
                >>= fun v_parsed ->
                Ok (PairsMap.add (k_outer, k_inner) v_parsed inner_acc))
             (Ok acc)
             inner_pairs
         | _ -> Error ("Expected a nested object for key: " ^ k_outer))
      (Ok PairsMap.empty)
      outer_pairs
  | _ -> Error "Expected a JSON object (Assoc)"
;;

let pairs_map_to_yojson value_to_yojson map =
  let bindings = PairsMap.bindings map in
  (* To go back to JSON, we need to group by the first key of the tuple *)
  let grouped =
    List.fold_left
      (fun acc ((k1, k2), v) ->
         let inner_list = List.assoc_opt k1 acc |> Option.value ~default:[] in
         (k1, (k2, value_to_yojson v) :: inner_list) :: List.remove_assoc k1 acc)
      []
      bindings
  in
  `Assoc (List.map (fun (k1, inner_fields) -> k1, `Assoc inner_fields) grouped)
;;

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

let string_map_to_yojson value_to_yojson map =
  let pairs = StringMap.bindings map |> List.map (fun (k, v) -> k, value_to_yojson v) in
  `Assoc pairs
;;

(* --- 2. Schema Definitions --- *)

(* The deriver will automatically use string_map_of_yojson for these! *)
type balances = int64 pairs_map [@@deriving yojson]
type variables_map = string string_map [@@deriving yojson]
type accounts_metadata = string pairs_map [@@deriving yojson]
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
  { balances : balances option [@default None]
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
