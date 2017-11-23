maximum' :: (Ord a) => [a] -> a
maximum' [] = error ("Maximum from empty list")
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' times x 
    | times < 1 = [] 
    | otherwise = x : replicate' (times - 1) x