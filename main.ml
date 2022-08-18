type expression = 
  | Sum of expression * expression
  | Product of expression * expression
  | Int of int
  | Float of float
  | Var of string
  | Power of expression * expression
;;

let simplifier = function
  | Sum (Int a, Int b) -> Int (a + b)
  | Sum (Float a, Float b) -> Float (a +. b)
  | Sum (Int a, Float b) -> Float ((Float.of_int a) +. b)
  | Sum (Float a, Int b) -> Float ((Float.of_int b) +. a)

  | Product (Int a, Int b) -> Int (a * b)
  | Product (Float a, Float b) -> Float (a *. b)
  | Product (Int a, Float b) -> Float ((Float.of_int a) *. b)
  | Product (Float a, Int b) -> Float ((Float.of_int b) *. a)

  | Power (Int a, Int b) -> Int (Int.of_float(Float.of_int(a) ** Float.of_int(b)))
  | Power (Float a, Float b) -> Float (a ** b)
  | Power (Int a, Float b) -> Float (Float.of_int(a) ** b)
  | Power (Float a, Int b) -> Float (a ** Float.of_int(b))

  | a -> a
;;