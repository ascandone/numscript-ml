open Numscript
open Common

let trim_suffix ~suffix s =
  if String.ends_with ~suffix s
  then String.sub s 0 (String.length s - String.length suffix)
  else s
;;

open Vm_register

let sf_pairs_to_run m =
  Specs_format.PairsMap.bindings m
  |> List.fold_left
       (fun acc (k, v) -> Run_state.PairsMap.add k v acc)
       Run_state.PairsMap.empty
;;

let sf_strmap_to_run m =
  Specs_format.StringMap.bindings m
  |> List.fold_left
       (fun acc (k, v) -> Run_state.StringMap.add k v acc)
       Run_state.StringMap.empty
;;

let inputs_balances_to_run (b : Inputs.balances) : int64 Run_state.PairsMap.t =
  Inputs.StringMap.fold
    (fun account asset_map acc ->
       Inputs.StringMap.fold
         (fun asset amount acc ->
            Run_state.PairsMap.add (account, asset) (Int64.of_float amount) acc)
         asset_map
         acc)
    b
    Run_state.PairsMap.empty
;;

let inputs_vars_to_run (v : Inputs.variables_map) : string Run_state.StringMap.t =
  Inputs.StringMap.bindings v
  |> List.fold_left
       (fun acc (k, v) -> Run_state.StringMap.add k v acc)
       Run_state.StringMap.empty
;;

let merge_balances a b = Specs_format.PairsMap.union (fun _ _ v -> Some v) a b
let merge_vars a b = Specs_format.StringMap.union (fun _ _ v -> Some v) a b

(* ---- Specs runner helpers ---- *)

let compute_movements postings =
  List.fold_left
    (fun acc (p : posting) ->
       let by_dest =
         Specs_format.StringMap.find_opt p.source acc
         |> Option.value ~default:Specs_format.StringMap.empty
       in
       let by_asset =
         Specs_format.StringMap.find_opt p.destination by_dest
         |> Option.value ~default:Specs_format.StringMap.empty
       in
       let current =
         Specs_format.StringMap.find_opt p.asset by_asset |> Option.value ~default:0.0
       in
       let by_asset' =
         Specs_format.StringMap.add p.asset (current +. Int64.to_float p.amount) by_asset
       in
       let by_dest' = Specs_format.StringMap.add p.destination by_asset' by_dest in
       Specs_format.StringMap.add p.source by_dest' acc)
    Specs_format.StringMap.empty
    postings
;;

let apply_postings initial_balances postings =
  List.fold_left
    (fun balances (p : posting) ->
       let key_src = p.source, p.asset in
       let key_dst = p.destination, p.asset in
       let src_bal =
         Specs_format.PairsMap.find_opt key_src balances |> Option.value ~default:0L
       in
       let dst_bal =
         Specs_format.PairsMap.find_opt key_dst balances |> Option.value ~default:0L
       in
       balances
       |> Specs_format.PairsMap.add key_src (Int64.sub src_bal p.amount)
       |> Specs_format.PairsMap.add key_dst (Int64.add dst_bal p.amount))
    initial_balances
    postings
;;

let format_posting source destination asset amount =
  Yojson.Safe.pretty_to_string
    (`Assoc
        [ "source", `String source
        ; "destination", `String destination
        ; "asset", `String asset
        ; "amount", `Int amount
        ])
;;

let check_postings (actual : posting list) expected fail =
  let n_actual = List.length actual in
  let n_expected = List.length expected in
  let posting_matches (a : posting) (e : Specs_format.posting) =
    a.source = e.source
    && a.destination = e.destination
    && a.asset = e.asset
    && a.amount = Int64.of_float e.amount
  in
  let mismatch =
    n_actual <> n_expected
    || List.exists2 (fun a e -> not (posting_matches a e)) actual expected
  in
  if mismatch
  then (
    let fmt_expected (e : Specs_format.posting) =
      format_posting e.source e.destination e.asset (int_of_float e.amount)
    in
    let fmt_actual (a : posting) =
      format_posting a.source a.destination a.asset (Int64.to_int a.amount)
    in
    let exp_lines = String.concat "\n" (List.map fmt_expected expected) in
    let got_lines = String.concat "\n" (List.map fmt_actual actual) in
    fail (Printf.sprintf "postings mismatch\nexpected:\n%s\ngot:\n%s" exp_lines got_lines))
;;

let check_end_balances_exact end_balances expected fail =
  Specs_format.PairsMap.iter
    (fun (account, asset) expected_amt ->
       let actual_amt =
         Specs_format.PairsMap.find_opt (account, asset) end_balances
         |> Option.value ~default:0L
       in
       if actual_amt <> expected_amt
       then
         fail
           (Printf.sprintf
              "end balance %s %s: expected %Ld, got %Ld"
              account
              asset
              expected_amt
              actual_amt))
    expected;
  Specs_format.PairsMap.iter
    (fun (account, asset) actual_amt ->
       if actual_amt <> 0L && not (Specs_format.PairsMap.mem (account, asset) expected)
       then
         fail
           (Printf.sprintf "unexpected end balance %s %s = %Ld" account asset actual_amt))
    end_balances
;;

let check_end_balances_include end_balances expected fail =
  Specs_format.PairsMap.iter
    (fun (account, asset) expected_amt ->
       let actual_amt =
         Specs_format.PairsMap.find_opt (account, asset) end_balances
         |> Option.value ~default:0L
       in
       if actual_amt <> expected_amt
       then
         fail
           (Printf.sprintf
              "end balance %s %s: expected %Ld, got %Ld"
              account
              asset
              expected_amt
              actual_amt))
    expected
;;

let check_movements actual_movements expected fail =
  Specs_format.StringMap.iter
    (fun source by_dest ->
       Specs_format.StringMap.iter
         (fun dest by_asset ->
            Specs_format.StringMap.iter
              (fun asset expected_amt ->
                 let actual_amt =
                   match Specs_format.StringMap.find_opt source actual_movements with
                   | None -> 0.0
                   | Some by_dest ->
                     (match Specs_format.StringMap.find_opt dest by_dest with
                      | None -> 0.0
                      | Some by_asset ->
                        Specs_format.StringMap.find_opt asset by_asset
                        |> Option.value ~default:0.0)
                 in
                 if actual_amt <> expected_amt
                 then
                   fail
                     (Printf.sprintf
                        "movement %s -> %s %s: expected %g, got %g"
                        source
                        dest
                        asset
                        expected_amt
                        actual_amt))
              by_asset)
         by_dest)
    expected
;;

let run_assertions ~initial_balances (tc : Specs_format.test_case) result =
  let failures = ref [] in
  let fail msg = failures := msg :: !failures in
  (match result, tc.expect_error_missing_funds with
   | Error (`Runtime MissingFunds), Some true -> ()
   | Error (`Runtime MissingFunds), _ -> fail "unexpected missing funds error"
   | Ok _, Some true -> fail "expected missing funds error but script succeeded"
   | Ok postings, _ ->
     (match tc.expect_postings with
      | Some expected -> check_postings postings expected fail
      | None -> ());
     (match tc.expect_end_balances with
      | Some expected ->
        check_end_balances_exact (apply_postings initial_balances postings) expected fail
      | None -> ());
     (match tc.expect_end_balances_include with
      | Some expected ->
        check_end_balances_include
          (apply_postings initial_balances postings)
          expected
          fail
      | None -> ());
     (match tc.expect_movements with
      | Some expected -> check_movements (compute_movements postings) expected fail
      | None -> ())
   | Error (`Compilation _), _ -> failwith "Unexpected compilation error"
   | Error (`Runtime _), _ -> failwith "Unexpected runtime error");
  List.rev !failures
;;

let run_specs_file specs_file_path any_failed =
  let numscript_file_path = trim_suffix ~suffix:".specs.json" specs_file_path in
  let specs =
    match Specs_format.parse_file specs_file_path with
    | Ok x -> x
    | Error err -> failwith err
  in
  let numscript_source =
    In_channel.with_open_text numscript_file_path In_channel.input_all
  in
  let ast = Syntax.Parser.parse numscript_source in
  let top_balances = Option.value specs.balances ~default:Specs_format.PairsMap.empty in
  let top_vars = Option.value specs.variables ~default:Specs_format.StringMap.empty in
  let has_focus =
    List.exists
      (fun (tc : Specs_format.test_case) -> tc.focus = Some true)
      specs.test_cases
  in
  List.iter
    (fun (tc : Specs_format.test_case) ->
       if tc.skip = Some true || (has_focus && tc.focus <> Some true)
       then Printf.printf "SKIP %s\n" tc.it
       else (
         let balances =
           merge_balances
             top_balances
             (Option.value tc.balances ~default:Specs_format.PairsMap.empty)
         in
         let vars =
           merge_vars
             top_vars
             (Option.value tc.variables ~default:Specs_format.StringMap.empty)
         in
         let result =
           Run.run_program
             ~vars:(sf_strmap_to_run vars)
             ~balances:(sf_pairs_to_run balances)
             ast
         in
         let failures = run_assertions ~initial_balances:balances tc result in
         if failures = []
         then Printf.printf "PASS %s\n" tc.it
         else (
           any_failed := true;
           Printf.printf "FAIL %s: %s\n" specs_file_path tc.it;
           List.iter (fun msg -> Printf.printf "  %s\n" msg) failures)))
    specs.test_cases
;;

let find_specs_files path =
  if Sys.file_exists path && Sys.is_directory path
  then (
    let rec scan dir =
      Sys.readdir dir
      |> Array.to_list
      |> List.concat_map (fun name ->
        let full = Filename.concat dir name in
        if Sys.is_directory full
        then scan full
        else if String.ends_with ~suffix:".num.specs.json" name
        then [ full ]
        else [])
    in
    scan path)
  else [ path ]
;;

(* ---- Subcommands ---- *)

let cmd_test path =
  let specs_files = find_specs_files path in
  let any_failed = ref false in
  List.iter (fun f -> run_specs_file f any_failed) specs_files;
  if !any_failed then exit 1
;;

let posting_to_json (p : posting) =
  `Assoc
    [ "source", `String p.source
    ; "destination", `String p.destination
    ; "asset", `String p.asset
    ; "amount", `Int (Int64.to_int p.amount)
    ]
;;

let cmd_run script_path =
  let inputs_path = script_path ^ ".inputs.json" in
  let inputs =
    if Sys.file_exists inputs_path
    then (
      match Inputs.inputs_of_yojson (Yojson.Safe.from_file inputs_path) with
      | Ok x -> x
      | Error err -> failwith err)
    else
      { Inputs.schema = None
      ; balances = None
      ; variables = None
      ; metadata = None
      ; feature_flags = None
      }
  in
  let source = In_channel.with_open_text script_path In_channel.input_all in
  let ast = Syntax.Parser.parse source in
  let balances =
    inputs.balances
    |> Option.fold ~none:Run_state.PairsMap.empty ~some:inputs_balances_to_run
  in
  let vars =
    inputs.variables
    |> Option.fold ~none:Run_state.StringMap.empty ~some:inputs_vars_to_run
  in
  match Run.run_program ~vars ~balances ast with
  | Error (`Runtime MissingFunds) ->
    Printf.eprintf "error: missing funds\n";
    exit 1
  | Error (`Runtime _) ->
    Printf.eprintf "error: runtime error\n";
    exit 1
  | Error (`Compilation _) ->
    Printf.eprintf "error: compilation error\n";
    exit 1
  | Ok postings ->
    let json = `Assoc [ "postings", `List (List.map posting_to_json postings) ] in
    print_string (Yojson.Safe.to_string json);
    print_newline ()
;;

let () =
  match Array.to_list Sys.argv with
  | _ :: "test" :: path :: _ -> cmd_test path
  | _ :: "run" :: path :: _ -> cmd_run path
  | _ ->
    Printf.eprintf
      "usage:\n\
      \  numscript-ml test <specs-file-or-folder>\n\
      \  numscript-ml run <script.num>\n";
    exit 1
;;
