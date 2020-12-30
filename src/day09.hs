import AdventAPI
import Advent.Utils (sinsert, findSumPair)
import Data.List (sort, delete)
import qualified Data.Vector as V

type XMAS = V.Vector Integer

-- | Go through the vector until the current number is not the sum
-- of two numbers in the sliding window. This window is kept sorted
-- to allow using 'findSumPair'.
findFstInvalid :: XMAS -> Int -> Integer
findFstInvalid xmas wSize = findFstInvalid' xmas wSize (sort . V.toList . V.take wSize $ xmas) wSize

findFstInvalid' :: XMAS -> Int -> [Integer] -> Int -> Integer
findFstInvalid' xmas wSize window i =
    case findSumPair value window of
        Just _  -> findFstInvalid' xmas wSize window' (succ i)
        Nothing -> value
    where value   = xmas V.! i
          window' = sinsert value . delete (xmas V.! (i - wSize)) $ window

-- | Start with the leftmost smallest range and progressively try larger ranges
-- until the sum is larger than invalid. Slide the range to the right and restart
-- the process. Repeat until a contiguous set of at least two elements whose sum
-- is invalid is found, or it doesn't exist.
findWeakness :: XMAS -> Integer -> [Integer]
findWeakness xmas invalid = findWeakness' xmas invalid (0, 1, xmas V.! 0 + xmas V.! 1)

findWeakness' :: XMAS -> Integer -> (Int,Int,Integer) -> [Integer]
findWeakness' xmas invalid (l, r, acc)
    | l >= V.length xmas - 2 = []
    | acc == invalid         = V.toList . V.slice l (r - l + 1) $ xmas
    | acc <  invalid         = findWeakness' xmas invalid (l, r + 1, acc + xmas V.! (r + 1))
    | acc >  invalid         = findWeakness' xmas invalid (l + 1, l + 2, xmas V.! (l + 1) + xmas V.! (l + 2))

main :: IO()
main = do
    contents <- AdventAPI.readInputDefaults 9

    let xmas     = map read . lines $ contents
        invalid  = findFstInvalid (V.fromList xmas) 25
        weakness = findWeakness (V.fromList xmas) invalid

    print invalid
    print $ minimum weakness + maximum weakness
