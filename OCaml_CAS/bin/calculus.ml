open Expression

let rec simplify = function
  | Add (Int a, Int b) -> Int (a + b)
  | Add (Float a, Float b) -> Float (a +. b)
  | Add (Int a, Float b) -> Float ((Float.of_int a) +. b)
  | Add (Float a, Int b) -> Float ((Float.of_int b) +. a)
  | Add (Int 0, a) -> simplify a
  | Add (a, Int 0) -> simplify a

  | Mult (Int a, Int b) -> Int (a * b)
  | Mult (Float a, Float b) -> Float (a *. b)
  | Mult (Int a, Float b) -> Float ((Float.of_int a) *. b)
  | Mult (Float a, Int b) -> Float ((Float.of_int b) *. a)
  | Mult (Int 0, _) -> Int 0
  | Mult (_, Int 0) -> Int 0
  | Mult (Int 1, a) -> simplify a
  | Mult (a, Int 1) -> simplify a

  | Fract (Float a, Float b) -> Float (a /. b)
  | Fract (Float a, Int b) -> Float (a /. (Float.of_int b))
  | Fract (Int a, Float b) -> Float ((Float.of_int a) /. b)
  | Fract (Int 0, _) -> Int 0

  | Power (Int a, b) -> Int (Int.of_float(Float.of_int(a) ** Float.of_int(b)))
  | Power (Float a, b) -> Float (a ** Float.of_int(b))

  | Ln (Exp a) -> simplify a
  | Exp (Ln a) -> simplify a
  | Mult (Exp a, Exp b) -> Exp(Add(simplify a, simplify b))
  | Add (Ln a, Ln b) -> Ln(Mult(simplify a, simplify b))

  | Add (Power (Sin a, 2), Power (Cos b, 2)) -> if a = b then Int 1 else Add (Power (Sin a, 2), Power (Cos b, 2))
  | Tan (Arctan a) -> simplify a

  | Add (a, b) -> Add (simplify a, simplify b)
  | Sub (a, b) -> Sub (simplify a, simplify b)
  | Mult (a, b) -> Mult (simplify a, simplify b)
  | Fract (a, b) -> Fract (simplify a, simplify b)
  | Power (a, b) -> Power (simplify a, b)
  | Ln a -> Ln (simplify a)
  | Exp a -> Exp (simplify a)
  | Sin a -> Sin (simplify a)
  | Cos a -> Cos (simplify a)
  | Arctan a -> Arctan (simplify a)
  | Tan a -> Tan (simplify a)
  | Int a -> Int a
  | Float a -> Float a
  | Var a -> Var a

;;

let rec derivative var = function
  | Int _ -> Int 0
  | Float _ -> Int 0
  | Var v -> if v = var then Int 1 else Int 0
  | Add (a, b) -> Add (derivative var a, derivative var b)
  | Sub (a, b) -> Sub (derivative var a, derivative var b)
  | Mult (a, b) -> Add (Mult (derivative var a, b), Mult (a, derivative var b))
  | Fract (a, b) -> Fract (Mult (derivative var a, b), Mult (a, derivative var b))
  | Power (e, b) -> Mult (Mult (Int b, (Mult (Int 1, Int 2))), derivative var e)

  | Exp a -> Mult (Exp a, derivative var a)
  | Ln a -> Mult (Fract (Int 1, a), derivative var a)
  | Sin a -> Mult (Cos a, derivative var a)
  | Cos a -> Mult (Sub (Int 0, Sin a), derivative var a)
  | Tan a -> Mult (Fract (Int 1, Power (Cos a, 2)), derivative var a)
  | Arctan a -> Mult (Fract (Int 1, Add (Int 1, Power (a, 2))), derivative var a)
;;