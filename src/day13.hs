import AdventAPI (readInputDefaults)

import Data.Function (on)
import Data.List (minimumBy)
import Data.List.Split (splitOn)

-- | Find the smallest multiple of 'n' that is bigger than 'limit'
smallestMultBiggerThan :: Int -> Int-> Int
smallestMultBiggerThan limit n = limit + n - (limit `mod` n)

-- | Find the earliest departure bus and time after 't'
findEarliestDeparture :: Int -> [Int] -> (Int, Int)
findEarliestDeparture t = minimumBy (compare `on` snd) . map ((,) <$> id <*> smallestMultBiggerThan t)

parseInput :: String -> (Int, [Int])
parseInput input = (t, ids)
    where ls = lines input
          t   = read . head $ ls
          ids = map read . filter (/= "x") . splitOn "," . (!! 1) $ ls  

main :: IO()
main = do
    contents <- AdventAPI.readInputDefaults 13

    let (t, ids) = parseInput contents
        depart   = findEarliestDeparture t ids

    print $ fst depart * (snd depart - t)
