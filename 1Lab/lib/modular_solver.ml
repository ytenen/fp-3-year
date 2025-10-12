open Matrix_utils

let generate_horizontal_sequences (m : matrix) : (int * int * int * int) list =
  let rows = List.length m in
  let cols = if rows > 0 then List.length (List.hd m) else 0 in
  let sequences = ref [] in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 4 do
      sequences :=
        ( List.nth (List.nth m i) j
        , List.nth (List.nth m i) (j + 1)
        , List.nth (List.nth m i) (j + 2)
        , List.nth (List.nth m i) (j + 3) )
        :: !sequences
    done
  done ;
  !sequences

let generate_vertical_sequences (m : matrix) : (int * int * int * int) list =
  let rows = List.length m in
  let cols = if rows > 0 then List.length (List.hd m) else 0 in
  let sequences = ref [] in
  for i = 0 to rows - 4 do
    for j = 0 to cols - 1 do
      sequences :=
        ( List.nth (List.nth m i) j
        , List.nth (List.nth m (i + 1)) j
        , List.nth (List.nth m (i + 2)) j
        , List.nth (List.nth m (i + 3)) j )
        :: !sequences
    done
  done ;
  !sequences

let generate_diagonal_sequences (m : matrix) : (int * int * int * int) list =
  let rows = List.length m in
  let cols = if rows > 0 then List.length (List.hd m) else 0 in
  let sequences = ref [] in
  for i = 0 to rows - 4 do
    for j = 0 to cols - 4 do
      (* Диагональ ↘ *)
      sequences :=
        ( List.nth (List.nth m i) j
        , List.nth (List.nth m (i + 1)) (j + 1)
        , List.nth (List.nth m (i + 2)) (j + 2)
        , List.nth (List.nth m (i + 3)) (j + 3) )
        :: !sequences
    done
  done ;
  for i = 3 to rows - 1 do
    for j = 0 to cols - 4 do
      sequences :=
        ( List.nth (List.nth m i) j
        , List.nth (List.nth m (i - 1)) (j + 1)
        , List.nth (List.nth m (i - 2)) (j + 2)
        , List.nth (List.nth m (i - 3)) (j + 3) )
        :: !sequences
    done
  done ;
  !sequences

let find_max_product (m : matrix) : int =
  let all_sequences =
    generate_horizontal_sequences m
    @ generate_vertical_sequences m
    @ generate_diagonal_sequences m
  in
  all_sequences
  |> List.map (fun (a, b, c, d) -> a * b * c * d)
  |> List.fold_left max min_int
