let gcd a b =
  let rec go a b = if b = 0L then a else go b (Int64.rem a b) in
  go (Int64.abs a) (Int64.abs b)
;;

let lcm a b = Int64.mul (Int64.div a (gcd a b)) b
