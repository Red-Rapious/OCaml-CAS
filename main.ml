type expression = 
  | Int of int
  | Float of float
  | Var of string
  | Sum of expression * expression
  | Product of expression * expression
  | Fract of expression * expression
  | Power of expression * int

  | Exp of expression
  | Ln of expression
  | Sin of expression
  | Cos of expression
  | Arctan of expression
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

  | Fract (Float a, Float b) -> Float (a /. b)
  | Fract (Float a, Int b) -> Float (a /. (Float.of_int b))
  | Fract (Int a, Float b) -> Float ((Float.of_int a) /. b)

  | Power (Int a, b) -> Int (Int.of_float(Float.of_int(a) ** Float.of_int(b)))
  | Power (Float a, b) -> Float (a ** Float.of_int(b))

  | Ln (Exp a) -> a
  | Exp (Ln a) -> a
  | Product (Exp a, Exp b) -> Exp(Sum(a, b))
  | Sum (Ln a, Ln b) -> Ln(Product(a, b))

  | Sum (Power (Sin a, 2), Power (Cos b, 2)) -> if a = b then Int 1 else Sum (Power (Sin a, 2), Power (Cos b, 2))

  | a -> a
;;

let rec derivative var = function
  | Int a -> Int 0
  | Float a -> Int 0
  | Var v -> if v = var then Int 1 else Int 0
  | Sum (a, b) -> Sum (derivative var a, derivative var b)
  | Product (a, b) -> Sum (Product (derivative var a, b), Product (a, derivative var b))
  | Fract (a, b) -> Fract (Product (derivative var a, b), Product (a, derivative var b))
  | Power (e, b) -> Product (Product (Int b, (Product (Int 1, Int 2))), derivative var e)

  | Exp a -> Product (Exp a, derivative var a)
  | Ln a -> Product (Fract (Int 1, a), derivative var a)
  | Sin a -> Product (Cos a, derivative var a)
  | Cos a -> Product (Product (Int (-1), Sin a), derivative var a)
  | Arctan a -> Product (Fract (Int 1, Sum (Int 1, Power (a, 2))), derivative var a)
;;