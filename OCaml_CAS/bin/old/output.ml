open Expression

let rec print_expression = function
  | Int a -> print_int a
  | Float a -> print_float a
  | Var a -> print_string a
  | Add (a, b) -> print_string "(" ; print_expression a ; print_string " + " ; print_expression b ; print_string ")"
  | Sub (a, b) -> print_string "(" ; print_expression a ; print_string " - " ; print_expression b ; print_string ")"
  | Mult (a, b) -> print_string "(" ; print_expression a ; print_string " * " ; print_expression b ; print_string ")"
  | Fract (a, b) -> print_string "(" ; print_expression a ; print_string " / " ; print_expression b ; print_string ")"
  | Power (a, b) -> print_string "(" ; print_expression a ; print_string " ^ " ; print_int b ; print_string ")"
  | Exp a -> print_string "exp(" ; print_expression a ; print_string ")"
  | Ln a -> print_string "ln(" ; print_expression a ; print_string ")"
  | Sin a -> print_string "sin(" ; print_expression a ; print_string ")"
  | Cos a -> print_string "cos(" ; print_expression a ; print_string ")"
  | Tan a -> print_string "tan(" ; print_expression a ; print_string ")"
  | Arctan a -> print_string "atan(" ; print_expression a ; print_string ")"
;;