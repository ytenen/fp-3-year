type matrix = int list list

val parse_line : string -> int list

val create_20x20_matrix : unit -> matrix

val get_element : matrix -> int -> int -> int option

val product4 : int -> int -> int -> int -> int
