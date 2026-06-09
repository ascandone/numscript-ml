type t = Virtual_instruction.t list -> Virtual_instruction.t list option

let rec merge opts code =
  match opts with
  | [] -> None
  | hd :: tl ->
    (match hd code with
     | Some new_code -> Some new_code
     | None -> merge tl code)
;;

let find_fixed_point opt code =
  let rec loop changed code =
    match opt code with
    | None -> if changed then Some code else None
    | Some new_code -> loop true new_code
  in
  loop false code
;;

let apply opt code =
  let instructions_list = Array.to_list code in
  let instructions_list_opt =
    Option.value ~default:instructions_list (opt instructions_list)
  in
  Array.of_list instructions_list_opt
;;
