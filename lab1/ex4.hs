-- 3.1
absInt :: Int -> Int 
absInt x = if x < 0 
    then -x 
    else x

-- 3.2
min2Int :: (Int, Int) -> Int 
min2Int (a, b) = if a < b
    then a
    else b

-- 3.4
min3Int :: (Int, Int, Int) -> Int 
min3Int (a, b, c) = 
    min2Int(min2Int (a,b), min2Int (b,c))

-- 3.5
--toUpper :: Char -> Char

--toLower :: Char -> Char

-- 3.6
isDigit :: Char -> Bool
isDigit ch = ch <= '9' && ch >= '0'

charToNum :: Char -> Int
charToNum ch = if isDigit ch 
    then ch - '0' 
    else -1


