{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, TupleSections #-}

import Advent.Megaparsec (decimal, readParsedLines, Parser)
import Advent.Coord.Grid (Coord)
import Data.Map (Map)
import Data.Map qualified as Map

type Line = (Coord, Coord)

-- | Parses "x1,y1 -> x2,y2" into ((y1,x1), (y2,x2)). Flips the
-- order of the coordinates to facilitate printing for debugging
-- (see showMap in Advent.Utils).
format :: Parser Line
format = (,) <$> point <* " -> " <*> point
    where
        point = flip (,) <$> decimal <* "," <*> decimal

-- |
-- >>> :main
-- 6710
-- 20121
main :: IO ()
main = do
    lines <- readParsedLines 2021 5 format

    let diagram1 = foldl (addLineToDiagram (const []))        Map.empty lines
        diagram2 = foldl (addLineToDiagram pointsInDiagonal)  Map.empty lines

    mapM_ (print . Map.size . Map.filter (> 1)) [diagram1, diagram2]

-- | Adds all the points in the line to the diagram map, using the given function
-- to determine the points if the line is not horizontal or vertical.
addLineToDiagram :: (Line -> [Coord]) -> Map Coord Int -> Line -> Map Coord Int
addLineToDiagram f diagram l@((y1, x1),(y2, x2))
    | x1 == x2  = Map.unionWith (+) diagram vertical
    | y1 == y2  = Map.unionWith (+) diagram horizontal
    | otherwise = Map.unionWith (+) diagram diagonal
    where
        horizontal = Map.fromList [((y1, x), 1) | x <- numbersBetween x1 x2]
        vertical   = Map.fromList [((y, x1), 1) | y <- numbersBetween y1 y2]
        diagonal   = Map.fromList . map (,1) $ f l

-- | Returns the list of integers between n1 and n2,
-- including n1 and n2.
-- >>> numbersBetween 3 7
-- [3,4,5,6,7]
--
-- >>> numbersBetween 4 1
-- [4,3,2,1]
numbersBetween :: (Ord a, Enum a) => a -> a -> [a]
numbersBetween n1 n2
    | n1 <= n2  = ns
    | otherwise = reverse ns
    where
        ns = [min n1 n2 .. max n1 n2]

-- | Returns all the points in a 45 degree diagonal,
-- including the edge points.
-- >>> pointsInDiagonal ((0,0), (3,3))
-- [(0,0),(1,1),(2,2),(3,3)]
--
-- >>> pointsInDiagonal ((5,5), (8,2))
-- [(5,5),(6,4),(7,3),(8,2)]
pointsInDiagonal :: Line -> [Coord]
pointsInDiagonal ((y1,x1),(y2,x2)) =
    zip (numbersBetween y1 y2) (numbersBetween x1 x2)
