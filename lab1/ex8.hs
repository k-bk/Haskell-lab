not' :: Bool -> Bool
not' b = case b of
            True -> False
            False -> True

absInt n = 
    case (n >= 0) of
        True -> n
        _    -> -n

isItTheAnswer :: String -> Bool
isItTheAnswer s = 
    case s of
        "Love" -> True
        _      -> False

or' :: (Bool, Bool) -> Bool
or' (a, b) = case (a, b) of
        (False, False) -> False
        _              -> True

and' :: (Bool, Bool) -> Bool
and' (a, b) =
    case (a, b) of
        (True, True) -> True
        _            -> False

xor' :: (Bool, Bool) -> Bool
xor' (a, b) = not' (a == b)

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
        | bmi <= 18.5 = "Under"
        | bmi <= 25.0 = "Normal?"
        | bmi <= 30.0 = "Fat"
        | otherwise = "Ugh"
        where bmi = bmiCalculate weight height

bmiCalculate :: (RealFloat a) => a -> a -> a 
bmiCalculate weight height = weight / height ^ 2

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

triangleArea :: (RealFloat a) => a -> a -> a -> a
triangleArea a b c = sqrt ((p - a) * (p - b) * (p - c))
        where p = (a + b + c) / 2

roots :: (Double a) => (a, a, a) -> (a, a)
roots (a, b, c) = 
        let d = b ^ 2 - 4 * a * c 
            e = 2 * a
        in ((-b + sqrt d) / e, (-b + sqrt d) / e)
