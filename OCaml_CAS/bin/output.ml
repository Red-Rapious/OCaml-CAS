open Expression

let rec print_expression = function
  | Int a -> print_int a
  | Var a -> print_string a
  | Add (a, b) -> print_string "(" ; print_expression a ; print_string " + " ; print_expression b ; print_string ")"
;;