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
           