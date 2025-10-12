open Matrix_utils

let find_max_product (m : matrix) : int =
  let rows = List.length m in
  let cols = if rows > 0 then List.length (List.hd m) else 0 in
  
  let rec check_directions i j max_val =
    if i >= rows then max_val
    else if j >= cols then check_directions (i + 1) 0 max_val
    else
      let new_max = ref max_val in
      
      if j + 3 < cols then (
        let a = List.nth (List.nth m i) j in
        let b = List.nth (List.nth m i) (j + 1) in
        let c = List.nth (List.nth m i) (j + 2) in
        let d = List.nth (List.nth m i) (j + 3) in
        new_max := max !new_max (a * b * c * d)
      );
      
      if i + 3 < rows then (
        let a = List.nth (List.nth m i) j in
        let b = List.nth (List.nth m (i + 1)) j in
        let c = List.nth (List.nth m (i + 2)) j in
        let d = List.nth (List.nth m (i + 3)) j in
        new_max := max !new_max (a * b * c * d)
      );
      
      if i + 3 < rows && j + 3 < cols then (
        let a = List.nth (List.nth m i) j in
        let b = List.nth (List.nth m (i + 1)) (j + 1) in
        let c = List.nth (List.nth m (i + 2)) (j + 2) in
        let d = List.nth (List.nth m (i + 3)) (j + 3) in
        new_max := max !new_max (a * b * c * d)
      );
      
      if i >= 3 && j + 3 < cols then (
        let a = List.nth (List.nth m i) j in
        let b = List.nth (List.nth m (i - 1)) (j + 1) in
        let c = List.nth (List.nth m (i - 2)) (j + 2) in
        let d = List.nth (List.nth m (i - 3)) (j + 3) in
        new_max := max !new_max (a * b * c * d)
      );
      
      check_directions i (j + 1) !new_max
  in
  
  if rows < 4 || cols < 4 then min_int
  else check_directions 0 0 min_int