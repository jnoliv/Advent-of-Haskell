{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, BlockArguments, TupleSections #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec
import Advent.Coord.Grid
import Advent.Life
import Advent.Math
import Advent.Utils

import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe

import Data.Map (Map)
import Data.Map qualified as Map

countVisible :: Map Coord Int -> Int
countVisible m = borders + nVisible
    where
        s@(nCols, nRows) = mapDimensions m
        borders          = nCols * nRows - (nCols - 1) * (nRows - 1)
        inside (y,x) _   = 0 < y && y < nCols && 0 < x && x < nRows
        internal         = Map.filterWithKey inside m
        nVisible         = Map.size . Map.filter id . Map.mapWithKey (visible m s) $ internal


visible :: Map Coord Int -> (Int, Int) -> Coord -> Int -> Bool
visible m (maxY, maxX) coord height =
    all (< height) allLeft || all (< height) allUp || all (< height) allRight || all (< height) allDown
    where
        outOfBounds :: Coord -> Bool
        outOfBounds (y,x) = y < 0 || maxY <= y || x < 0 || maxX <= x

        allInDirection :: (Coord -> Coord) -> Coord -> [Coord]
        allInDirection d = takeWhile (not . outOfBounds) . tail . iterate d

        getHeight :: [Coord] -> [Int]
        getHeight = map (fromJust . (flip Map.lookup m))

        allLeft  = getHeight $ allInDirection left  coord
        allUp    = getHeight $ allInDirection up    coord
        allRight = getHeight $ allInDirection right coord
        allDown  = getHeight $ allInDirection down  coord

maxScenic :: Map Coord Int -> Int
maxScenic m = maximum . map snd $ Map.toList scenicScores
    where
        s@(nCols, nRows) = mapDimensions m
        scenicScores     = Map.mapWithKey (scenic m s) m

scenic :: Map Coord Int -> (Int, Int) -> Coord -> Int -> Int
scenic m (maxY, maxX) coord height = allLeft * allUp * allRight * allDown
    where
        outOfBounds :: Coord -> Bool
        outOfBounds (y,x) = y < 0 || maxY <= y || x < 0 || maxX <= x

        getHeight :: [Coord] -> [Int]
        getHeight = map (fromJust . (flip Map.lookup m))

        allInDirection :: (Coord -> Coord) -> Coord -> [Coord]
        allInDirection d = takeWhile (not . outOfBounds) . tail . iterate d

        allLeft  = length . takeWhileInclusive (< height) . getHeight $ allInDirection left  coord
        allUp    = length . takeWhileInclusive (< height) . getHeight $ allInDirection up    coord
        allRight = length . takeWhileInclusive (< height) . getHeight $ allInDirection right coord
        allDown  = length . takeWhileInclusive (< height) . getHeight $ allInDirection down  coord

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []

-- |
-- >>> :main
-- 1835
-- 263670
main :: IO ()
main = do
    input <- readAsMap (Just . digitToInt) <$> readInputDefaults 2022 8

    print $ countVisible input
    print $ maxScenic input
