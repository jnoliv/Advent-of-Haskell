{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec (readParsed, decimal, sepBy)

-- |
-- >>> :main
-- 336721
-- 91638945
main :: IO ()
main = do
    crabs <- readParsed 2021 7 (decimal `sepBy` ",")

    let left  = minimum crabs
        right = maximum crabs

    let fuelByPos  = map (totalFuel distance    crabs) [left .. right]
        fuelByPos2 = map (totalFuel incDistance crabs) [left .. right]

    mapM_ (print . minimum) [fuelByPos, fuelByPos2]

-- | Determines the total fuel required for all crabs to move to pos,
-- using d to determine the fuel cost of moving between two points.
-- totalFuel distance [16,1,2,0,4,2,7,1,2,14] 2
-- 37
-- 
-- >>> totalFuel incDistance [16,1,2,0,4,2,7,1,2,14] 5
-- 168
totalFuel :: (Int -> Int -> Int) -> [Int] -> Int -> Int
totalFuel d crabs pos = foldl (\acc crab -> acc + d pos crab) 0 crabs

-- | Regular distance between two positions (in one dimension).
-- >>> distance 3 7
-- 4
-- 
-- >>> distance 9 2
-- 7
distance :: Int -> Int -> Int
distance a b = abs (a - b)

-- | Incremental distance between two positions (in one dimension)
-- where each step has 1 more weight than the previous.
-- >>> incDistance 3 7
-- 10
--
-- >>> incDistance 9 2
-- 28
incDistance :: Int -> Int -> Int
incDistance a b = ((n + 1) * n) `div` 2
    where n = distance a b
