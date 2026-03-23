type t =
  | Int of int
  | Asset of string
  | Account of string
  | Monetary of string * int

type type_err =
  { expected : string
  ; got : t
  }

let expect_asset = function
  | Asset x -> Ok x
  | got -> Error { expected = "asset"; got }
;;

let expect_number = function
  | Int x -> Ok x
  | got -> Error { expected = "number"; got }
;;

let expect_monetary = function
  | Monetary (m, v) -> Ok (m, v)
  | got -> Error { expected = "monetary"; got }
;;

let expect value expecting =
  match expecting value with
  | Ok x -> x
  | Error _ -> failwith "Invalid type"
;;
