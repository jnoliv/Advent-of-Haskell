{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid (Coord)
import Advent.Graph (aStar)
import Advent.Utils (readAsMap)
import Data.Char (ord)
import Data.Maybe (fromJust)

import Data.Map (Map)
import Data.Map qualified as Map

parseHeightmap :: String -> (Map Coord Int, Coord, Coord)
parseHeightmap s = (Map.map p hm, start, goal)
    where
        p 'S' = 0
        p 'E' = 25
        p  c  = ord c - ord 'a'

        hm    = readAsMap (\c -> Just c) s
        start = fst . head . filter ((== 'S') . snd) $ Map.toList hm
        goal  = fst . head . filter ((== 'E') . snd) $ Map.toList hm

fNeigh :: Map Coord Int -> Coord -> [Coord] -> [Coord]
fNeigh hm cur neighbours = filter ((<= curHeight + 1) . getHeight) neighbours
    where
        getHeight p = fromJust $ Map.lookup p hm
        curHeight   = getHeight cur

-- |
-- >>> :main
-- 394
-- 388
main :: IO ()
main = do
    (heightmap, start, goal) <- parseHeightmap <$> readInputDefaults 2022 12

    let minSteps = fromJust $ aStar fNeigh heightmap [start] goal

        minElevations   = map fst . Map.toList $ Map.filter (== 0) heightmap
        minStepsFromAny = fromJust $ aStar fNeigh heightmap minElevations goal

    print minSteps
    print minStepsFromAny
