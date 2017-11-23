import Data.Char

myFun x = 2 * x

fiveToPower_ :: Integer -> Integer
fiveToPower_ = (^) 5 

_ToPower5 :: Num a => a -> a
_ToPower5 =  flip (^) 5

substrNFrom5 :: Num a => a -> a
substrNFrom5 = (5 -)

substr5From_ :: Num a => a -> a
substr5From_ = flip2 (-) 5

flip2 :: (a -> b -> c) -> (b -> a -> c)
flip2 f x y = f y x

flip3 :: (a -> b -> c -> d) -> (c -> b -> a -> d)
flip3 f a b c = f c b a

isPalindrome :: [Char] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome s = head s == last s && isPalindrome (init (tail s))

getElemAtIdx :: [a] -> Int -> a
getElemAtIdx xs x = head (drop x xs)

capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (x:xs) = toUpper x : capitalize xs 

isPrime :: Int -> Bool 
isPrime n = head (takeWhile (<=n) primes) == n

--countPrimes :: Int -> Int
--countPrimes x = length [i | i <- [2..x], isPrime i]

primes :: [Int]
primes = eratoSieve [2..]
  where
   eratoSieve :: [Int] -> [Int]
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

allEqual :: Eq a => [a] -> Bool
allEqual [_]        = True -- allEqual [1,1] = True, allEqual [1,2] = False
allEqual [a,b]      = a == b
allEqual (a:b:tl) = a == b && allEqual (b:tl)

allEqual' :: Eq a => [a] -> Bool
allEqual' xs = [x | x <- xs, x /= head xs] == []