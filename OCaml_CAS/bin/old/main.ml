open Expression
open Calculus
open Output

let expr = Mult (Var "y", Var "x")
let _ = print_expression (simplify (derivative "x" expr)) ; print_string "\n";;
let expr = Add (Int 0, Int 1);;
let _ = print_expression (simplify expr) ; print_string "\n";;