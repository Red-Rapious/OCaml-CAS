open Expression
open Calculus
open Output

let expr = Mult (Var "y", Var "x")
let _ = print_expression (simplify (derivative "x" expr));;