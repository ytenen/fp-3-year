open Matrix_utils

let find_max_product (m : matrix) : int =
  let rows = List.length m in
  let cols = if rows > 0 then List.length (List.hd m) else 0 in
  
  let all_products = 
    List.mapi (fun i row ->
      List.mapi (fun j _ ->
        let products = ref [] in
        
        if j + 3 < cols then
          products := 
            (List.nth row j * List.nth row (j + 1) * 
             List.nth row (j + 2) * List.nth row (j + 3)) :: !products
        ;
        
        if i + 3 < rows then
          products := 
            (List.nth row j * 
             List.nth (List.nth m (i + 1)) j *
             List.nth (List.nth m (i + 2)) j *
             List.nth (List.nth m (i + 3)) j) :: !products
        ;
        
        if i + 3 < rows && j + 3 < cols then
          products := 
            (List.nth row j * 
             List.nth (List.nth m (i + 1)) (j + 1) *
             List.nth (List.nth m (i + 2)) (j + 2) *
             List.nth (List.nth m (i + 3)) (j + 3)) :: !products
        ;
        
        if i >= 3 && j + 3 < cols then
          products := 
            (List.nth row j * 
             List.nth (List.nth m (i - 1)) (j + 1) *
             List.nth (List.nth m (i - 2)) (j + 2) *
             List.nth (List.nth m (i - 3)) (j + 3)) :: !products
        ;
        
        !products
      ) row
    ) m
    |> List.flatten
    |> List.flatten
  in
  
  List.fold_left max min_int all_products