open Numscript

let trim_suffix ~suffix s =
  if String.ends_with ~suffix s
  then String.sub s 0 (String.length s - String.length suffix)
  else s

let sf_pairs_to_run m =
  Specs_format.PairsMap.bindings m
  |> List.fold_left (fun acc (k, v) -> Run.PairsMap.add k v acc) Run.PairsMap.empty

let sf_strmap_to_run m =
  Specs_format.StringMap.bindings m
  |> List.fold_left (fun acc (k, v) -> Run.StringMap.add k v acc) Run.StringMap.empty

let merge_balances a b =
  Specs_format.PairsMap.union (fun _ _ v -> Some v) a b

let merge_vars a b =
  Specs_format.StringMap.union (fun _ _ v -> Some v) a b

let compute_movements postings =
  List.fold_left
    (fun acc (p : Run.posting) ->
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
         Specs_format.StringMap.add p.asset (current +. float_of_int p.amount) by_asset
       in
       let by_dest' = Specs_format.StringMap.add p.destination by_asset' by_dest in
       Specs_format.StringMap.add p.source by_dest' acc)
    Specs_format.StringMap.empty
    postings

let check_postings (actual : Run.posting list) expected fail =
  let n_actual = List.length actual in
  let n_expected = List.length expected in
  if n_actual <> n_expected
  then fail (Printf.sprintf "postings: expected %d, got %d" n_expected n_actual)
  else
    List.iter2
      (fun (a : Run.posting) (e : Specs_format.posting) ->
         if a.source <> e.source
         || a.destination <> e.destination
         || a.asset <> e.asset
         || a.amount <> int_of_float e.amount
         then
           fail
             (Printf.sprintf
                "posting mismatch: expected {%s -> %s %s %g}, got {%s -> %s %s %d}"
                e.source
                e.destination
                e.asset
                e.amount
                a.source
                a.destination
                a.asset
                a.amount))
      actual
      expected

let apply_postings initial_balances postings =
  List.fold_left
    (fun balances (p : Run.posting) ->
       let key_src = p.source, p.asset in
       let key_dst = p.destination, p.asset in
       let src_bal =
         Specs_format.PairsMap.find_opt key_src balances |> Option.value ~default:0
       in
       let dst_bal =
         Specs_format.PairsMap.find_opt key_dst balances |> Option.value ~default:0
       in
       balances
       |> Specs_format.PairsMap.add key_src (src_bal - p.amount)
       |> Specs_format.PairsMap.add key_dst (dst_bal + p.amount))
    initial_balances
    postings

let check_end_balances_exact end_balances expected fail =
  Specs_format.PairsMap.iter
    (fun (account, asset) expected_amt ->
       let actual_amt =
         Specs_format.PairsMap.find_opt (account, asset) end_balances
         |> Option.value ~default:0
       in
       if actual_amt <> expected_amt
       then
         fail
           (Printf.sprintf
              "end balance %s %s: expected %d, got %d"
              account
              asset
              expected_amt
              actual_amt))
    expected;
  Specs_format.PairsMap.iter
    (fun (account, asset) actual_amt ->
       if actual_amt <> 0
          && not (Specs_format.PairsMap.mem (account, asset) expected)
       then
         fail
           (Printf.sprintf "unexpected end balance %s %s = %d" account asset actual_amt))
    end_balances

let check_end_balances_include end_balances expected fail =
  Specs_format.PairsMap.iter
    (fun (account, asset) expected_amt ->
       let actual_amt =
         Specs_format.PairsMap.find_opt (account, asset) end_balances
         |> Option.value ~default:0
       in
       if actual_amt <> expected_amt
       then
         fail
           (Printf.sprintf
              "end balance %s %s: expected %d, got %d"
              account
              asset
              expected_amt
              actual_amt))
    expected

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

let run_assertions ~initial_balances (tc : Specs_format.test_case) result =
  let failures = ref [] in
  let fail msg = failures := msg :: !failures in
  (match result, tc.expect_error_missing_funds with
   | Error Run.MissingFunds, Some true -> ()
   | Error Run.MissingFunds, _ -> fail "unexpected missing funds error"
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
        check_end_balances_include (apply_postings initial_balances postings) expected fail
      | None -> ());
     (match tc.expect_movements with
      | Some expected -> check_movements (compute_movements postings) expected fail
      | None -> ()));
  List.rev !failures

let () =
  match Array.to_list Sys.argv with
  | _ :: specs_file_path :: _ ->
    let numscript_file_path = trim_suffix ~suffix:".specs.json" specs_file_path in
    let specs =
      match Specs_format.parse_file specs_file_path with
      | Ok x -> x
      | Error err -> failwith err
    in
    let numscript_source =
      In_channel.with_open_text numscript_file_path In_channel.input_all
    in
    let ast = Parser.parse numscript_source in
    let top_balances = Option.value specs.balances ~default:Specs_format.PairsMap.empty in
    let top_vars = Option.value specs.variables ~default:Specs_format.StringMap.empty in
    let has_focus =
      List.exists (fun (tc : Specs_format.test_case) -> tc.focus = Some true) specs.test_cases
    in
    let any_failed = ref false in
    List.iter
      (fun (tc : Specs_format.test_case) ->
         if tc.skip = Some true || (has_focus && tc.focus <> Some true)
         then Printf.printf "SKIP %s\n" tc.it
         else begin
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
           else begin
             any_failed := true;
             Printf.printf "FAIL %s\n" tc.it;
             List.iter (fun msg -> Printf.printf "  %s\n" msg) failures
           end
         end)
      specs.test_cases;
    if !any_failed then exit 1
  | _ -> failwith "Needs a `path` arg"
