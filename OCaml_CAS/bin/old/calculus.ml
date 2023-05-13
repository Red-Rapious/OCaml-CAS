open Expression

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
    | Ln a, Ln b -> Ln(Mult(simplify a, simplify b))

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
    | Ln a, Ln b -> Ln(Fract(simplify a, simplify b))

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
    | _, 0 -> Int 1

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
    | Int 0 -> Int 0

    | a -> Tan (a)
  in
  let simplify_arctan x =
    let x = simplify x in match x with
    | Int 0 -> Int 0

    | a -> Arctan (a)
  in
  let simplify_sin x =
    let x = simplify x in match x with
    | Int 0 -> Int 0

    | a -> Sin (a)
  in
  let simplify_cos x =
    let x = simplify x in match x with
    | Int 0 -> Int 1

    | a -> Cos (a)
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
  | Arctan a -> simplify_arctan a
  | Sin a -> simplify_sin a
  | Cos a -> simplify_cos a

  | Int a -> Int a
  | Float a -> Float a
  | Var a -> Var a
;;

let rec evaluate expr var value = 
  let expr_value = match expr with
  | Int a -> Int a
  | Float a -> Float a
  | Var a -> if a = var then Float value else Var a
  | Add (a, b) -> Add (evaluate a var value, evaluate b var value)
  | Sub (a, b) -> Sub (evaluate a var value, evaluate b var value)
  | Mult (a, b) -> Mult (evaluate a var value, evaluate b var value)
  | Fract (a, b) -> Fract (evaluate a var value, evaluate b var value)
  | Power (a, b) -> Power (evaluate a var value, b)
  | Ln a -> Ln (evaluate a var value)
  | Exp a -> Exp (evaluate a var value)
  | Tan a -> Tan (evaluate a var value)
  | Arctan a -> Arctan (evaluate a var value)
  | Sin a -> Sin (evaluate a var value)
  | Cos a -> Cos (evaluate a var value)
in simplify expr_value;;