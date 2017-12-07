qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  = filter (<= x) 
   rightPart = filter (> x) 

mSort :: Ord a => [a] -> [a]
mSort [] = []
mSort [a] = [a]
mSort x  = merge (mSort (take (half x) x)) (mSort (drop (half x) x))
 where half x = div (length x) 2

merge :: Ord a => [a] -> [a] -> [a]
merge [] x          = x
merge x []          = x 
merge (x:xs) (y:ys) = if x > y then 
                        y : (merge (x:xs) ys)
                      else
                        x : (merge xs (y:ys))