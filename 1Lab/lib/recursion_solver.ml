open Matrix_utils

let find_max_product (m : matrix) : int =
  let rows = List.length m in
  let cols = if rows > 0 then List.length (List.hd m) else 0 in
  
  let rec process_cell i j =
    if i >= rows then min_int
    else if j >= cols then process_cell (i + 1) 0
    else
      let current_max = ref min_int in
      
      if j + 3 < cols then (
        let a = List.nth (List.nth m i) j in
        let b = List.nth (List.nth m i) (j + 1) in
        let c = List.nth (List.nth m i) (j + 2) in
        let d = List.nth (List.nth m i) (j + 3) in
        current_max := max !current_max (a * b * c * d)
      );
      
      if i + 3 < rows then (
        let a = List.nth (List.nth m i) j in
        let b = List.nth (List.nth m (i + 1)) j in
        let c = List.nth (List.nth m (i + 2)) j in
        let d = List.nth (List.nth m (i + 3)) j in
        current_max := max !current_max (a * b * c * d)
      );
      
      if i + 3 < rows && j + 3 < cols then (
        let a = List.nth (List.nth m i) j in
        let b = List.nth (List.nth m (i + 1)) (j + 1) in
        let c = List.nth (List.nth m (i + 2)) (j + 2) in
        let d = List.nth (List.nth m (i + 3)) (j + 3) in
        current_max := max !current_max (a * b * c * d)
      );
      
      if i >= 3 && j + 3 < cols then (
        let a = List.nth (List.nth m i) j in
        let b = List.nth (List.nth m (i - 1)) (j + 1) in
        let c = List.nth (List.nth m (i - 2)) (j + 2) in
        let d = List.nth (List.nth m (i - 3)) (j + 3) in
        current_max := max !current_max (a * b * c * d)
      );
      
      max !current_max (process_cell i (j + 1))
  in
  
  if rows < 4 || cols < 4 then min_int
  else process_cell 0 0