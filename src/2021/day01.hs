import Advent.Megaparsec (decimal, readParsedLines)
import Advent.Utils (count)

-- |
-- >>> :main
-- 1195
-- 1235
main :: IO ()
main = do
    numbers <- readParsedLines 2021 1 decimal

    print $ countIncreases 1 numbers
    print $ countIncreases 3 numbers

-- | Counts the number of times the sum of all elements in
-- a sliding window of size w increases.
-- >>> countIncreases 1 [0, 1, 2, 3, 4, 2, 0]
-- 4
--
-- >>> countIncreases 3 [0, 1, 2, 3, 4, 2, 0]
-- 2
countIncreases :: Ord a => Int -> [a] -> Int
countIncreases w ns = count id $ zipWith (<) ns (drop w ns)
