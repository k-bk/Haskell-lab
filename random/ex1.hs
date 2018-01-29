-- /show
import System.Random

-- show
data Coin = Heads | Tails deriving (Eq, Show, Enum, Bounded)
-- /show

instance Random Coin where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g

-- show
-- Number of samples to take
count = 10000

-- Function to process our random sequence
process :: [Coin] -> (Int, Int)
process cs = (length cs, length (filter (== Heads) cs))

-- Function to display the running value.
display:: (Int, Int) -> String
display (coins, heads) = "We got " ++ (show $ 100.0 * fromIntegral heads / fromIntegral coins) ++ "% heads."

main = do
  g <- newStdGen
  putStrLn . display . process . take count $ randoms g