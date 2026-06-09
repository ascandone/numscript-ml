type t = Reg of string

let eq (Reg s1) (Reg s2) = String.equal s1 s2
let show (Reg s) = s
let pp fmt (Reg s) = Format.fprintf fmt "%s" s
let int = Reg "int"
let monetary = Reg "monetary"
let string = Reg "string"
let portion = Reg "portion"
