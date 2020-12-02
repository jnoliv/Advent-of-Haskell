import qualified Common.AdventAPI as AdventAPI

import Data.List (sort)

-- | Finds a pair in the given list that sums to the given number.
-- Uses the two pointer technique to do so, which is probably not the
-- most efficient way to do this in Haskell. This assumes a sorted input
-- list! Check the algorithm here:
-- https://www.geeksforgeeks.org/two-pointers-technique/
findPair :: Integer -> Int -> Int -> [Integer] -> Maybe (Integer, Integer)
findPair _ _ _ [] = Nothing
findPair sum l r list
        | l >= r = Nothing
        | curSum > sum  = findPair sum l (r - 1) list
        | curSum < sum  = findPair sum (l + 1) r list
        | curSum == sum = Just (left, right)
    where
        left  = list !! l
        right = list !! r
        curSum = left + right

-- | Finds a triplet in the given list that sums to the given number.
-- Fixes a number and then uses the two pointer technique to see if any pair
-- matches the new sum (the given sum minus the fixed number). Tries it for
-- every number until a match or the end of the list. Assumes a sorted input
-- list!
findTriplet :: Integer -> [Integer] -> Maybe (Integer, Integer, Integer)
findTriplet _ [] = Nothing
findTriplet sum (x:xs) =
    let newSum = sum - x
        lastInd = length xs - 1 
    in case findPair newSum 0 lastInd xs of
        Just (y,z) -> Just (x,y,z)
        Nothing    -> findTriplet sum xs

main :: IO()
main = do
    contents <- AdventAPI.readInput 1 "../session-cookie.txt" "../input"
    let expenses = sort . map read . lines $ contents
    let size = length expenses

    case findPair 2020 0 (size - 1) expenses of
        Just (x,y) -> putStrLn $ "Pair multiplication: " ++ show (x * y)
        Nothing    -> putStrLn "No two entries sum to 2020"

    case findTriplet 2020 expenses of
        Just (x,y,z) -> putStrLn $ "Triplet multiplication: " ++ show (x * y * z)
        Nothing      -> putStrLn "No three entries sum to 2020"
