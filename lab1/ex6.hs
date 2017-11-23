absInt :: Int -> Int
absInt n | n > 0 = n
         | otherwise = -n

sgn :: Int -> Int
sgn n | n > 0 = 1
      | n < 0 = -1
      | otherwise = 0
      
min3Int :: (Int, Int, Int) -> Int
min3Int (x, y, z) | x < y = if x < z then x else z
                  | otherwise = if y < z then y else z

