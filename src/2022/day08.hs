{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid (Coord, up, right, down, left)
import Advent.Utils (takeUntil, readAsMap, mapDimensions)
import Data.Char (digitToInt)
import Data.Maybe (fromJust)

import Data.Map (Map)
import Data.Map qualified as Map

type Forest = Map Coord Int

countVisible :: Forest -> Int
countVisible m = (Map.size borders) + (Map.size internalVisible)
    where
        mapSize@(nCols, nRows) = mapDimensions m
        inside (y,x) _         = (0 < y && y < nCols) && (0 < x && x < nRows)
        (internal, borders)    = Map.partitionWithKey inside m
        internalVisible        = Map.filterWithKey (visible m mapSize) internal

visible :: Forest -> (Int, Int) -> Coord -> Int -> Bool
visible m mapSize coord maxHeight = any (all (< maxHeight)) heightsPerDirection
    where
        treesPerDirection   = map (\d -> treesInDirection mapSize d coord) [left, up, right, down]
        heightsPerDirection = (map.map) (height m) treesPerDirection

treesInDirection :: (Int, Int) -> (Coord -> Coord) -> Coord -> [Coord]
treesInDirection (nCols, nRows) d = takeWhile inside . tail . iterate d
    where inside (y,x) = (0 <= y && y < nCols) && (0 <= x && x < nRows)

height :: Forest -> Coord -> Int
height m = fromJust . (flip Map.lookup m)

maxScenic :: Forest -> Int
maxScenic m = maximum . map snd $ Map.toList scenicScores
    where
        mapSize      = mapDimensions m
        scenicScores = Map.mapWithKey (scenic m mapSize) m

scenic :: Forest -> (Int, Int) -> Coord -> Int -> Int
scenic m mapSize coord maxHeight = product visiblePerDirection
    where
        treesPerDirection   = map (\d -> treesInDirection mapSize d coord) [left, up, right, down]
        heightsPerDirection = (map.map) (height m) treesPerDirection
        visiblePerDirection = map (length . takeUntil (< maxHeight)) heightsPerDirection

-- |
-- >>> :main
-- 1835
-- 263670
main :: IO ()
main = do
    input <- readAsMap (Just . digitToInt) <$> readInputDefaults 2022 8

    print $ countVisible input
    print $ maxScenic input
