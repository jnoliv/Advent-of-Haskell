{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, TupleSections #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid (row, neighbours4, Coord, down, left, right, up)
import Advent.Utils (findDefininingPoints, readAsMap)

import Control.Applicative (liftA2, empty)
import Data.Bifunctor (second)
import Data.Bool (bool)
import Data.List (find, delete, union)
import Data.Maybe (fromJust)

import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

neighbours :: Map Coord Char -> Coord -> [Coord]
neighbours trails pos = case tile of
    '^' -> [up    pos]
    '>' -> [right pos]
    'v' -> [down  pos]
    '<' -> [left  pos]
    _   -> neighbours' trails pos
    where
        tile = trails ! pos

neighbours' :: Map Coord Char -> Coord -> [Coord]
neighbours' trails pos = filter (`Map.member` trails) (neighbours4 pos)

simplify :: (Map Coord Char -> Coord -> [Coord]) -> Map Coord Char -> Map Coord (Map Coord Int)
simplify getNeighbours trails = Map.fromList $ map (second Map.fromList . adjacents) nodes
    where
        nodes = filter ((/= 2) . length . getNeighbours trails) $ Map.keys trails

        adjacents node = (node,) $ findAdjacent Set.empty (node `delete` nodes) 0 node

        findAdjacent :: Set Coord -> [Coord] -> Int -> Coord -> [(Coord,Int)]
        findAdjacent seen nodes dist cur
            | cur `elem` nodes = [(cur, dist)]
            | otherwise        = foldr1 union $ map (findAdjacent seen' nodes (dist + 1)) nexts
            where
                nexts = filter (`Set.notMember` seen) (getNeighbours trails cur)
                seen' = Set.insert cur seen

findMax :: Map Coord (Map Coord Int) -> Coord -> Coord -> Int
findMax adjacency start goal = find Set.empty start
    where
        find seen cur
            | cur == goal = 0
            | null nexts  = 0
            | otherwise   = maximum $ map (\(n,w) -> w + find seen' n) nexts
            where
                seen' = Set.insert cur seen
                nexts = filter ((`Set.notMember` seen) . fst) $ Map.assocs (adjacency ! cur)

-- |
-- >>> :main
-- 2414
-- 6598
main :: IO ()
main = do
    trails <- readAsMap (liftA2 (bool empty) pure (/= '#')) <$> readInputDefaults 2023 23

    let positions       = Map.keys trails
        ((r0,_),(r1,_)) = findDefininingPoints positions

        start = fromJust $ find ((== r0) . row) positions
        goal  = fromJust $ find ((== r1) . row) positions

    print $ findMax (simplify neighbours  trails) start goal
    print $ findMax (simplify neighbours' trails) start goal
