type expression = 
  | Int of int
  | Var of string
  | Add of expression * expression
;;

