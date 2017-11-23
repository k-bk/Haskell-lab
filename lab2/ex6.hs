fib :: (Num a, Eq a) => a -> a
fib n =
 if n == 0 || n == 1 then n
 else fib (n - 2) + fib (n - 1)

fib2 :: Int -> Int
fib2 n = fibs !! n
 where
   fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]

--fordr' _ acc [x]    = acc
--fordr' f acc (x:xs) = f x (foldr' f acc xs)

sum' :: Num a => [a] -> a
sum' = foldr (+) 0

prod' :: Num a => [a] -> a -- prod' [1,2,3] = 6
prod' = foldr (*) 1 

--length' :: [a] -> Int -- length' [1,1,1,1] = 4
--length' = foldr' () 

or' :: [Bool] -> Bool -- or' [True, False, True] = True
or' = foldr (||) False

and' :: [Bool] -> Bool -- and' [True, False, True] = False
and' = foldr (&&) True

--elem' :: Eq a => a -> [a] -> Bool -- elem' 3 [1,2,3] = True

--doubleAll :: Num t => [t] -> [t] -- double doubleAll [1,2] = [2,4]

--squareAll :: Num t => [t] -> [t] -- double squareAll [2,3] = [4,9]

--selectEven :: Integral t => [t] -> [t] -- selectEven [1,2,3] = [2]