let trim_suffix ~suffix s =
  if String.ends_with ~suffix s
  then (
    let new_len = String.length s - String.length suffix in
    String.sub s 0 new_len)
  else s
;;

let () =
  match Array.to_list Sys.argv with
  | _ :: specs_file_path :: _ ->
    let numscript_file_path = trim_suffix ~suffix:".specs.json" specs_file_path in
    let _specs =
      match Numscript.Specs_format.parse_file specs_file_path with
      | Ok x ->
        print_endline "parsed specs";
        x
      | Error err -> failwith err
    in
    let numscript_source =
      In_channel.with_open_text numscript_file_path In_channel.input_all
    in
    let _ast = Numscript.Parser.parse numscript_source in
    print_endline "parsed ast";
    ()
  | _ -> failwith "Needs a `path` arg"
;;
