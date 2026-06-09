type t = Virtual_instruction.t list -> Virtual_instruction.t list option

val merge : t list -> t
val find_fixed_point : t -> t
val apply : t -> Virtual_instruction.t array -> Virtual_instruction.t array
