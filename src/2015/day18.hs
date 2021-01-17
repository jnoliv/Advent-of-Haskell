{-# LANGUAGE ImportQualifiedPost #-}

import AdventAPI (readInputDefaults)
import Advent.Coord.Grid (Coord, neighbours8)
import Advent.Life
import Advent.Utils (readAsSet)
import Data.Set (Set)
import Data.Set qualified as Set

neighbours :: Int -> Coord -> [Coord]
neighbours size = filter f . neighbours8
    where f (r,c) = 0 <= r && r < size && 0 <= c && c < size

rules :: Bool -> Int -> Bool
rules True  n = n == 2 || n == 3
rules False n = n == 3

evolve' :: Int -> Set Coord -> Set Coord
evolve' size lights = foldr Set.insert (evolve (neighbours size) rules lights) corners
    where corners = [(r,c) | r <- [0, size - 1], c <- [0, size - 1]]

-- |
-- >>> :main
-- 768
-- 781
main :: IO ()
main = do
    input <- readInputDefaults 2015 18

    let lights  = readAsSet (== '#') input
        size    = length $ lines input 
        frames  = iterate (evolve (neighbours size) rules) lights
        frames' = iterate (evolve' size) lights

    print . countAlive $ frames  !! 100
    print . countAlive $ frames' !! 100
