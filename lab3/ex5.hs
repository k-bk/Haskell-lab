import Data.List

sortDesc :: Ord a => [a] -> [a]
sortDesc xs = reverse (sort xs)

are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
are2FunsEqAt f g [] = True
are2FunsEqAt f g (x : xs) = 
  if f x /= g x 
    then False 
    else are2FunsEqAt f g xs

infixl 9 >.>
  (>.>) :: (a -> b) -> (b -> c) -> (a -> c)
  g >.> f = g . f 