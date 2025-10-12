open Alcotest
open Lab1

let test_all_implementations () =
  let matrix = Matrix_utils.create_20x20_matrix () in
  let expected = 70600674 in
  let results =
    [ ("Tailrec", Tailrec_solver.find_max_product matrix)
    ; ("Recursion", Recursion_solver.find_max_product matrix)
    ; ("Modular", Modular_solver.find_max_product matrix)
    ; ("Map", Map_solver.find_max_product matrix)
    ; ("Loop", Loop_solver.find_max_product matrix) ]
  in
  List.iter
    (fun (name, result) -> check int (name ^ " implementation") expected result)
    results

let () =
  run "Lab1 Tests"
    [ ( "implementations"
      , [ test_case "All implementations give same result" `Quick
            test_all_implementations ] ) ]
