open Lab1

let () =
  let matrix = Matrix_utils.create_20x20_matrix () in
  
  Printf.printf "=== Результаты различных реализаций ===\n";
  Printf.printf "Хвостовая рекурсия: %d\n" (Tailrec_solver.find_max_product matrix);
  Printf.printf "Обычная рекурсия: %d\n" (Recursion_solver.find_max_product matrix);
  Printf.printf "Модульный стиль: %d\n" (Modular_solver.find_max_product matrix);
  Printf.printf "С использованием map: %d\n" (Map_solver.find_max_product matrix);
  Printf.printf "Императивные циклы: %d\n" (Loop_solver.find_max_product matrix);
  Printf.printf "==============================\n"