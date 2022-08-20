open Expression

let rec simplify_old = function
  | Add (Int a, Int b) -> Int (a + b)
  | Add (Float a, Float b) -> Float (a +. b)
  | Add (Int a, Float b) -> Float ((Float.of_int a) +. b)
  | Add (Float a, Int b) -> Float ((Float.of_int b) +. a)

  | Add (Int 0, a) -> simplify_old a
  | Add (a, Int 0) -> simplify_old a
  | Add (Float 0.0, a) -> simplify_old a
  | Add (a, Float 0.0) -> simplify_old a

  | Mult (Int a, Int b) -> Int (a * b)
  | Mult (Float a, Float b) -> Float (a *. b)
  | Mult (Int a, Float b) -> Float ((Float.of_int a) *. b)
  | Mult (Float a, Int b) -> Float ((Float.of_int b) *. a)

  | Mult (Int 0, _) -> Int 0
  | Mult (_, Int 0) -> Int 0
  | Mult (Float 0.0, _) -> Int 0
  | Mult (_, Float 0.0) -> Int 0
  | Mult (Int 1, a) -> simplify_old a
  | Mult (a, Int 1) -> simplify_old a
  | Mult (Float 1.0, a) -> simplify_old a
  | Mult (a, Float 1.0) -> simplify_old a

  | Fract (Float a, Float b) -> Float (a /. b)
  | Fract (Float a, Int b) -> Float (a /. (Float.of_int b))
  | Fract (Int a, Float b) -> Float ((Float.of_int a) /. b)
  | Fract (Int 0, _) -> Int 0

  | Power (Int a, b) -> Int (Int.of_float(Float.of_int(a) ** Float.of_int(b)))
  | Power (Float a, b) -> Float (a ** Float.of_int(b))

  | Ln (Exp a) -> simplify_old a
  | Exp (Ln a) -> simplify_old a
  | Mult (Exp a, Exp b) -> Exp(Add(simplify_old a, simplify_old b))
  | Add (Ln a, Ln b) -> Ln(Mult(simplify_old a, simplify_old b))

  | Add (Power (Sin a, 2), Power (Cos b, 2)) -> if a = b then Int 1 else Add (Power (Sin a, 2), Power (Cos b, 2))
  | Tan (Arctan a) -> simplify_old a

  | Add (a, b) -> Add (simplify_old a, simplify_old b)
  | Sub (a, b) -> Sub (simplify_old a, simplify_old b)
  | Mult (a, b) -> Mult (simplify_old a, simplify_old b)
  | Fract (a, b) -> Fract (simplify_old a, simplify_old b)
  | Power (a, b) -> Power (simplify_old a, b)
  | Ln a -> Ln (simplify_old a)
  | Exp a -> Exp (simplify_old a)
  | Sin a -> Sin (simplify_old a)
  | Cos a -> Cos (simplify_old a)
  | Arctan a -> Arctan (simplify_old a)
  | Tan a -> Tan (simplify_old a)
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
  | Fract (a, b) -> Fract (Sub (Mult (derivative var a, b), Mult (a, derivative var b)), Power (b, 2))
  | Power (e, b) -> Mult (Mult (Int b, (Mult (Int 1, Int 2))), derivative var e)

  | Exp a -> Mult (Exp a, derivative var a)
  | Ln a -> Mult (Fract (Int 1, a), derivative var a)
  | Sin a -> Mult (Cos a, derivative var a)
  | Cos a -> Mult (Sub (Int 0, Sin a), derivative var a)
  | Tan a -> Mult (Fract (Int 1, Power (Cos a, 2)), derivative var a)
  | Arctan a -> Mult (Fract (Int 1, Add (Int 1, Power (a, 2))), derivative var a)
;;

let rec simplify e =
  let simplify_add a b = 
    let a, b = simplify a, simplify b in match a, b with
    | Int 0, x -> x
    | x, Int 0 -> x
    | Float 0.0, x -> x
    | x, Float 0.0 -> x
    | Int x, Int y -> Int (x + y)
    | Float x, Float y -> Float (x +. y)
    | Int x, Float y -> Float ((Float.of_int x) +. y)
    | Float x, Int y -> Float (x +. (Float.of_int y))
    | Power (Sin a, 2), Power (Cos b, 2) -> if a = b then Int 1 else Add (Power (Sin a, 2), Power (Cos b, 2))
    | Ln a, Ln b -> Ln(Mult(simplify_old a, simplify_old b))

    | x, y -> Add (x, y)
  in
  let simplify_sub a b = 
    let a, b = simplify a, simplify b in match a, b with
    | x, Int 0 -> x
    | x, Float 0.0 -> x
    | Int x, Int y -> Int (x - y)
    | Float x, Float y -> Float (x -. y)
    | Int x, Float y -> Float ((Float.of_int x) -. y)
    | Float x, Int y -> Float (x -. (Float.of_int y))
    | Ln a, Ln b -> Ln(Fract(simplify_old a, simplify_old b))

    | x, y -> Add (x, y)
  in
  let simplify_mult x y = 
    let x, y = simplify x, simplify y in match x, y with
    | Int a, Int b -> Int (a * b)
    | Float a, Float b -> Float (a *. b)
    | Int a, Float b -> Float ((Float.of_int a) *. b)
    | Float a, Int b -> Float (a /. (Float.of_int b))
    | Int 0, _ -> Int 0
    | _, Int 0 -> Int 0
    | Float 0.0, _ -> Int 0
    | _, Float 0.0 -> Int 0
    | Int 1, a -> a
    | a, Int 1 -> a
    | Float 1.0, a -> a
    | a, Float 1.0 -> a
    | Exp a, Exp b -> Exp(Add(a, b))

    | a, b -> Mult (a, b)
  in
  let simplify_fract x y =
    let x, y = simplify x, simplify y in match x, y with
    | Int 0, _ -> Int 0
    | Float a, Float b -> Float (a /. b)
    | Float a, Int b -> Float (a /. (Float.of_int b))
    | Int a, Float b -> Float ((Float.of_int a) /. b)
    | Exp a, Exp b -> Exp(Sub(a, b))

    | a, b -> Fract (a, b)
  in
  let simplify_power x y =
    let x = simplify x in match x, y with
    | Int a, b -> Int (Int.of_float(Float.of_int(a) ** Float.of_int(b)))
    | Float a, b -> Float (a ** Float.of_int(b))

    | a, b -> Power (a, b)
  in
  let simplify_ln x =
    let x = simplify x in match x with
    | Exp a ->  a
    | Int 1 -> Int 0

    | a -> Ln (a)
  in
  let simplify_exp x =
    let x = simplify x in match x with
    | Ln a -> a
    | Int 0 -> Int 1

    | a -> Exp (a)
  in
  let simplify_tan x =
    let x = simplify x in match x with
    | Arctan a -> a
    | a -> Tan (a)
  in
  match e with
  | Add (a, b) -> simplify_add a b
  | Sub (a, b) -> simplify_sub a b
  | Mult (a, b) -> simplify_mult a b
  | Fract (a, b) -> simplify_fract a b
  | Power (a, b) -> simplify_power a b
  | Ln a -> simplify_ln a
  | Exp a -> simplify_exp a
  | Tan a -> simplify_tan a

  | Int a -> Int a
  | Float a -> Float a
  | Var a -> Var a
  | Sin a -> Sin (simplify a)
  | Cos a -> Cos (simplify a)
  | Arctan a -> Arctan (simplify a)
;;