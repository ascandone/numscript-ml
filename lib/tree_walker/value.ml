type t =
  | Int of int64
  | Portion of int64 * int64
  | Asset of string
  | String of string
  | Account of string
  | Monetary of string * int64

type type_err =
  { expected : string
  ; got : t
  }

let expect_asset = function
  | Asset x -> Ok x
  | got -> Error { expected = "asset"; got }
;;

let expect_account = function
  | Account x -> Ok x
  | got -> Error { expected = "account"; got }
;;

let expect_number = function
  | Int x -> Ok x
  | got -> Error { expected = "number"; got }
;;

let expect_monetary = function
  | Monetary (m, v) -> Ok (m, v)
  | got -> Error { expected = "monetary"; got }
;;

let expect_portion = function
  | Portion (num, den) -> Ok (num, den)
  | got -> Error { expected = "portion"; got }
;;

let expect value expecting =
  match expecting value with
  | Ok x -> x
  | Error _ -> failwith "Invalid type"
;;
