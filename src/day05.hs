import AdventAPI
import Data.List (sort)
import Utils (binToDec)

-- | Parse a character of the binary space partioning into a bit
bspToBit :: Char -> Int
bspToBit c
    | c == 'F' || c == 'L' = 0
    | c == 'B' || c == 'R' = 1

-- | Find the gap in a sorted list of seats
findSeat :: [Int] -> Int
findSeat (n:m:ss) = if n == pred m then findSeat (m:ss) else succ n
findSeat _ = error "No free seat exists"

main :: IO()
main = do
    contents <- AdventAPI.readInputDefaults 5

    let seats = sort . map (binToDec . map bspToBit) . lines $ contents

    print . maximum $ seats
    print . findSeat $ seats
