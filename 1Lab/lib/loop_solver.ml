open Matrix_utils

let find_max_product (m : matrix) : int =
  let rows = List.length m in
  let cols = if rows > 0 then List.length (List.hd m) else 0 in
  let max_val = ref min_int in
  if rows < 4 || cols < 4 then !max_val
  else (
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        ( if j + 3 < cols then
            let prod =
              List.nth (List.nth m i) j
              * List.nth (List.nth m i) (j + 1)
              * List.nth (List.nth m i) (j + 2)
              * List.nth (List.nth m i) (j + 3)
            in
            max_val := max !max_val prod ) ;
        ( if i + 3 < rows then
            let prod =
              List.nth (List.nth m i) j
              * List.nth (List.nth m (i + 1)) j
              * List.nth (List.nth m (i + 2)) j
              * List.nth (List.nth m (i + 3)) j
            in
            max_val := max !max_val prod ) ;
        ( if i + 3 < rows && j + 3 < cols then
            let prod =
              List.nth (List.nth m i) j
              * List.nth (List.nth m (i + 1)) (j + 1)
              * List.nth (List.nth m (i + 2)) (j + 2)
              * List.nth (List.nth m (i + 3)) (j + 3)
            in
            max_val := max !max_val prod ) ;
        if i >= 3 && j + 3 < cols then
          let prod =
            List.nth (List.nth m i) j
            * List.nth (List.nth m (i - 1)) (j + 1)
            * List.nth (List.nth m (i - 2)) (j + 2)
            * List.nth (List.nth m (i - 3)) (j + 3)
          in
          max_val := max !max_val prod
      done
    done ;
    !max_val )
