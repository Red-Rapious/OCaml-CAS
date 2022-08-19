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