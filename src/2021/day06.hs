{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec (readParsed, decimal, sepBy)
import Data.Map (Map)
import Data.Map qualified as Map

r, r0 :: Int
r  = 6
r0 = 8

-- |
-- >>> :main
-- 350149
-- 1590327954513
main :: IO ()
main = do
    input <- readParsed 2021 6 (decimal `sepBy` ",")

    let fish = foldl (\m f -> Map.insertWith (+) f 1 m) Map.empty input
        gens = iterate step fish

    mapM_ (print . count . (gens !!)) [80, 256]

-- | Steps from one generation to the next.
-- >>> step $ Map.fromList [(0,4),(1,3),(2,5),(3,3),(4,2),(5,2),(6,1),(7,1),(8,1)]
-- fromList [(0,3),(1,5),(2,3),(3,2),(4,2),(5,1),(6,5),(7,1),(8,4)]
step :: Map Int Int -> Map Int Int
step gen = birth $ Map.mapKeysWith (+) f gen
    where
        f 0 = r
        f n = n - 1
        
        birth newgen = case Map.lookup 0 gen of
            Just n  -> Map.insert r0 n newgen
            Nothing -> newgen

-- | Counts the number of fish in the given generation,
-- i.e., sums all the values in the map.
-- >>> count $ Map.fromList [(0,3),(1,5),(2,3),(3,2),(4,2),(5,1),(6,5),(7,1),(8,4)]
-- 26
count :: Map Int Int -> Int
count = Map.foldl (+) 0
