{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid ((.*), (.+), (.-), Coord)
import Advent.Utils (combinations, mapDimensions, inside, readAsMap)
import Data.Function (on)
import Data.List (sortBy, groupBy)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

findAntinodes :: Map Coord Char -> ([Coord] -> [Coord]) -> Set Coord
findAntinodes antennas f = Set.fromList $ concatMap (concatMap f) pairs
    where
        grouped = groupBy ((==) `on` snd) . sortBy (compare `on` snd) $ Map.toList antennas
        pairs   = map (combinations 2 . map fst) grouped

antinodes :: ([Coord] -> [Coord]) -> [Coord] -> [Coord]
antinodes f [a,b] = pos ++ neg
    where
        d = b .- a

        pos = f [a .- (n .* d) | n <- [0..]]
        neg = f [b .+ (n .* d) | n <- [0..]]

-- |
-- >>> :main
-- 426
-- 1359
main :: IO ()
main = do
    input <- readInputDefaults 2024 8

    let cityMap  = readAsMap Just input
        antennas = Map.filter (/= '.') cityMap

    print . Set.size . findAntinodes antennas . antinodes $ filter (inside cityMap) . take 1
    print . Set.size . findAntinodes antennas . antinodes $ takeWhile (inside cityMap)
