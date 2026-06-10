type t

val string : t
val int : t
val portion : t
val monetary : t
val show : t -> string
val pp : Format.formatter -> t -> unit
val eq : t -> t -> bool
val of_expr_typ : Common.expr_typ -> t
