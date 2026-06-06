let rec gcd a b = if Int64.equal b 0L then Int64.abs a else gcd b (Int64.rem a b)

type t =
  { num : int64
  ; den : int64
  }

let zero = { num = 0L; den = 1L }
let one = { num = 1L; den = 1L }

let create ~num ~den =
  assert (0L <> den);
  if num = 0L
  then zero
  else (
    let d = gcd num den in
    let num = Int64.div num d in
    let den = Int64.div den d in
    { num; den })
;;

let num p = p.num
let den p = p.den

let add p1 p2 =
  let left_term = Int64.mul p1.num p2.den in
  let right_term = Int64.mul p2.num p1.den in
  create ~num:(Int64.add left_term right_term) ~den:(Int64.mul p1.den p2.den)
;;

let sub p1 p2 =
  let left_term = Int64.mul p1.num p2.den in
  let right_term = Int64.mul p2.num p1.den in
  create ~num:(Int64.sub left_term right_term) ~den:(Int64.mul p1.den p2.den)
;;

let mul p1 p2 = create ~num:(Int64.mul p1.num p2.num) ~den:(Int64.mul p1.den p2.den)
let inv p = create ~num:p.den ~den:p.num
let div p1 p2 = mul p1 (inv p2)
