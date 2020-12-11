import AdventAPI
import Data.List (sort)
import Utils (findSumPair)

-- | Finds a triplet in the given list that sums to the given number.
-- Fixes a number and then uses the two pointer technique to see if any pair
-- matches the new sum (the given sum minus the fixed number). Tries it for
-- every number until a match or the end of the list. Assumes a sorted input
-- list!
findTriplet :: Integer -> [Integer] -> Maybe (Integer, Integer, Integer)
findTriplet _ [] = Nothing
findTriplet sum (x:xs) =
    case findSumPair (sum - x) xs of
        Just (y,z) -> Just (x,y,z)
        Nothing    -> findTriplet sum xs

main :: IO()
main = do
    contents <- AdventAPI.readInputDefaults 1
    let expenses = sort . map read . lines $ contents

    case findSumPair 2020 expenses of
        Just (x,y) -> print     $ x * y
        Nothing    -> putStrLn "No two entries sum to 2020"

    case findTriplet 2020 expenses of
        Just (x,y,z) -> print    $ x * y * z
        Nothing      -> putStrLn "No three entries sum to 2020"
