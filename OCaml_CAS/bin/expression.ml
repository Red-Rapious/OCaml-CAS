type expression = 
  | Int of int
  | Float of float
  | Var of string
  | Add of expression * expression
  | Sub of expression * expression
  | Mult of expression * expression
  | Fract of expression * expression
  | Power of expression * int

  | Exp of expression
  | Ln of expression
  | Sin of expression
  | Cos of expression
  | Tan of expression
  | Arctan of expression
;;

(* let authorized_variable_name = regexp "[a-zA-Z_][a-zA-Z0-9_]*";; *)

let rec check_expression_validity = function
  | Int _ -> true
  | Float _ -> true
  | Var _ -> true (*string_match authorized_variable_name a 0*)
  | Add (a, b) -> check_expression_validity a && check_expression_validity b
  | Sub (a, b) -> check_expression_validity a && check_expression_validity b
  | Mult (a, b) -> check_expression_validity a && check_expression_validity b
  | Fract (a, b) -> if b = Int 0 
    then (print_endline "[Error] Divide by zero" ; false)
    else (check_expression_validity a && check_expression_validity b)
  | Power (a, _) -> check_expression_validity a
  | Exp a -> check_expression_validity a
  | Ln (Int a) -> if a <= 0
      then (print_endline "[Error] Negative or null value in Ln" ; false)
      else true
  | Ln (Float a) -> if a <= 0.0
      then (print_endline "[Error] Negative or null value in Ln" ; false)
      else true
  | Ln a -> check_expression_validity a
  | Sin a -> check_expression_validity a
  | Cos a -> check_expression_validity a
  | Tan a -> check_expression_validity a (* À modifier *)
  | Arctan a -> check_expression_validity a
;;