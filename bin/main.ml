let () =
  let specs_file_path = "./examples/simple.num.specs.json" in
  let numscript_file_path = "./examples/simple.num" in
  let _specs =
    match Numscript.Specs_format.parse_file specs_file_path with
    | Ok x ->
      print_endline "parsed specs";
      x
    | Error _ -> failwith "err"
  in
  let numscript_source =
    In_channel.with_open_text numscript_file_path In_channel.input_all
  in
  let _ast = Numscript.Parser.parse numscript_source in
  print_endline "parsed ast";
  ()
;;
