{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid (neighbours4, Coord)
import Advent.Utils (readAsMap)
import Data.Char (digitToInt)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

countTrails :: Map Coord Int -> Coord -> [Coord]
countTrails topoMap curr = case (paths, nextHeight) of
    ([],10) -> [curr]
    ([], _) -> []
    (ps, _) -> concatMap (countTrails topoMap) ps
    where
        nextHeight = succ $ Map.findWithDefault (-1) curr topoMap

        neighbours = filter (`Map.member` topoMap) $ neighbours4 curr
        withHeight = map (\n -> (n, Map.findWithDefault (-1) n topoMap)) neighbours
        paths      = map fst $ filter ((== nextHeight) . snd) withHeight

-- |
-- >>> :main
-- 552
-- 1225
main :: IO ()
main = do
    input <- readAsMap (Just . digitToInt) <$> readInputDefaults 2024 10

    let maybeTrailheads = Map.keys $ Map.filter (== 0) input

    print . sum . map (Set.size . Set.fromList . countTrails input) $ maybeTrailheads
    print . sum . map (length                  . countTrails input) $ maybeTrailheads
