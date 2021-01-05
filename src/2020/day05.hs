import AdventAPI
import Advent.Utils (readBin)
import Data.List (sort)

-- | Parse a character of the binary space partioning into a bit
bspToBit :: Char -> Char
bspToBit c
    | c == 'F' || c == 'L' = '0'
    | c == 'B' || c == 'R' = '1'

-- | Find the gap in a sorted list of seats
findSeat :: [Int] -> Int
findSeat (n:m:ss) = if n == pred m then findSeat (m:ss) else succ n
findSeat _ = error "No free seat exists"

-- |
-- >>> :main
-- 892
-- 625
main :: IO()
main = do
    contents <- readInputDefaults 2020 5

    let seats = sort . map (readBin . map bspToBit) . lines $ contents

    print . maximum $ seats
    print . findSeat $ seats
