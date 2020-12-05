import qualified Common.AdventAPI as AdventAPI

import Data.List (sort)

bspToBit :: Char -> Int
bspToBit c
    | c == 'F' || c == 'L' = 0
    | c == 'B' || c == 'R' = 1

binToDec :: [Int] -> Int
binToDec = foldl (\acc bit -> 2*acc + bit) 0

findSeat :: [Int] -> Int
findSeat (n:m:ss) = if n == pred m then findSeat (m:ss) else succ n
findSeat _ = -1

main :: IO()
main = do
    contents <- AdventAPI.readInput 5 "../session-cookie.txt" "../input"

    let seats = sort . map (binToDec . map bspToBit) . lines $ contents

    print . maximum $ seats
    print . findSeat $ seats
