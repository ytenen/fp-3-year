(* Тип матрицы *)
type matrix = int list list

(* Парсинг строки с числами в список int *)
val parse_line : string -> int list

(* Создание тестовой матрицы 20x20 *)
val create_20x20_matrix : unit -> matrix

(* Получение элемента матрицы с проверкой границ *)
val get_element : matrix -> int -> int -> int option

(* Вычисление произведения 4 чисел *)
val product4 : int -> int -> int -> int -> int