type expression = 
  | Int of int
  | Float of float
  | Var of string
  | Sum of expression * expression
  | Product of expression * expression
  | Power of expression * expression

  | Exp of expression
  | Ln of expression
  | Sin of expression
  | Cos of expression
;;

let simplify = function
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

  | Ln (Exp a) -> a
  | Exp (Ln a) -> a
  | Product (Exp a, Exp b) -> Exp(Sum(a, b))
  | Sum (Ln a, Ln b) -> Ln(Product(a, b))

  | Sum (Power (Sin a, Int 2), Power (Cos b, Int 2)) -> if a = b then Int 1 else Sum (Power (Sin a, Int 2), Power (Cos b, Int 2))


  | a -> a
;;