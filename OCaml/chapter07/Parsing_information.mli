type t

val t_of_lexbuf : Lexing.lexbuf -> t

val line_of_t : t -> int
val start_position_of_t : t -> int
val filename_of_t : t -> string
