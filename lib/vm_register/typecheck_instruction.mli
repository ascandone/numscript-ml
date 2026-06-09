include module type of Typecheck_instruction_intf

type typecheck_state

val create_state : unit -> typecheck_state
val push_instruction : typecheck_state -> Virtual_instruction.t -> unit
