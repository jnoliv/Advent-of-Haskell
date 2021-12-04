import Advent.Megaparsec (alphaNumChar, many, readParsedLines)
import Advent.Utils (count, readBin)
import Control.Arrow (Arrow((&&&)))
import Data.Function (on)
import Data.List (transpose)

-- |
-- >>> :main
-- 852500
-- 1007985
main :: IO ()
main = do
    numbers <- readParsedLines 2021 3 (many alphaNumChar)

    let bits = transpose numbers

    let gamma   = map mostCommonBit  bits
        epsilon = map leastCommonBit bits

    print $ ((*) `on` readBin) gamma epsilon

    let o2  = rating mostCommonBit  numbers
        co2 = rating leastCommonBit numbers

    print $ ((*) `on` readBin) o2 co2

-- | Return the most / least common bit in the given list,
-- defaulting to 1 / 0 when both bits are equally common.
-- >>> map mostCommonBit ["0101010", "0101011", "010101"]
-- "011"
--
-- >>> map leastCommonBit ["0101010", "0101011", "010101"]
-- "100"
mostCommonBit, leastCommonBit :: [Char] -> Char
mostCommonBit  = fCommonBit (>) '1'
leastCommonBit = fCommonBit (<) '0'

fCommonBit :: (Int -> Int -> Bool) -> Char -> [Char] -> Char
fCommonBit f def l
    | num0s `f` num1s = '0'
    | num1s `f` num0s = '1'
    | otherwise       = def
    where
        num0s = count (== '0') l
        num1s = count (== '1') l

-- | Repeatedly filter the numbers by applying the bit selection
-- criteria and keeping only the numbers with that selected bit
-- in the current position. Advances the bit position until only
-- one number remains, which is the return value.
rating :: Eq b => ([b] -> b) -> [[b]] -> [b]
rating  crit numbers = rating' crit numbers 0
    where
        rating' crit numbers i
            | n > 1  = rating' crit filtered (i + 1)
            | n == 1 = head filtered
            where
                bit      = crit $ map (!!i) numbers
                filtered = filter ((== bit) . (!!i)) numbers
                n        = length filtered
