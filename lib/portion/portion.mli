type t

val create : num:int64 -> den:int64 -> t
val zero : t
val one : t
val num : t -> int64
val den : t -> int64
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val inv : t -> t
val pp : Format.formatter -> t -> unit
