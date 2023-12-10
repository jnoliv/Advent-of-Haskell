{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid ((.+), (.-), down, left, neighbours4, right, turnRight, up, Coord)
import Advent.Utils (findDefininingPoints, readAsMap)

import Data.Bool (bool)
import Data.List (nub)
import Control.Applicative (liftA2, empty)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

adjacentPipes :: Char -> Coord -> [Coord]
adjacentPipes '|' = sequence [  up,   down]
adjacentPipes '-' = sequence [ left, right]
adjacentPipes 'L' = sequence [   up, right]
adjacentPipes 'J' = sequence [   up,  left]
adjacentPipes '7' = sequence [ left,  down]
adjacentPipes 'F' = sequence [right,  down]
adjacentPipes  _  = sequence []

walkPipes :: Map Coord (Char, [Coord]) -> Coord -> Coord -> (Map Coord Int, Set Coord)
walkPipes pipes start next = walkPipes' visited0 next dir0 1 (dists0, maybeEnclosed0)
    where
        visited0       = Set.singleton start
        dir0           = next .- start
        dists0         = Map.singleton start 0
        maybeEnclosed0 = Set.singleton (start .+ turnRight dir0)

        -- Walk the pipe, keeping track of distances and adding all tiles to the right of current to maybeEnclosed.
        -- At the end, filter maybeEnclosed to guarantee no elements are part of the pipe.
        walkPipes' :: Set Coord -> Coord -> Coord -> Int -> (Map Coord Int, Set Coord) -> (Map Coord Int, Set Coord)
        walkPipes' visited cur dir dist (dists, maybeEnclosed)
            | length nexts == 1 = walkPipes' visited' next dir' (dist + 1) (dists', maybeEnclosed'')
            | otherwise         = (dists', Set.filter (`Map.notMember` dists') maybeEnclosed')
            where
                nexts = filter (`Set.notMember` visited) $ snd (pipes Map.! cur)
                next  = head nexts

                visited'        = Set.insert cur visited
                dir'            = next .- cur
                dists'          = Map.insert cur dist dists

                -- Add the point to the right based on current and next directions because of turns.
                -- Note how the otherwise case above uses maybeEnclose', since there is no next direction then.
                maybeEnclosed'  = Set.insert (cur .+ turnRight dir ) maybeEnclosed
                maybeEnclosed'' = Set.insert (cur .+ turnRight dir') maybeEnclosed'

floodFill :: (Coord -> Bool) -> Set Coord -> Set Coord
floodFill canFill = flip floodFill' Set.empty . Set.elems
    where
        floodFill' :: [Coord] -> Set Coord -> Set Coord
        floodFill' [] visited = visited
        floodFill' curs visited = floodFill' nexts visited'
            where
                visited' = foldr Set.insert visited curs
                nexts    = filter (\c -> c `Set.notMember` visited' && canFill c) . nub $ concatMap neighbours4 curs

-- |
-- >>> :main
-- 7086
-- 317
main :: IO ()
main = do
    input <- readAsMap (liftA2 (bool empty) pure (/= '.')) <$> readInputDefaults 2023 10

    let pipes = Map.mapWithKey (\k v -> (v, adjacentPipes v k)) input
        start = fst . head . Map.toList $ Map.filter (== 'S') input

        [next1, next2] = filter ((start `elem`) . snd . (pipes Map.!)) $ neighbours4 start

        (dists1, maybeEnclosed1) = walkPipes pipes start next1
        (dists2, maybeEnclosed2) = walkPipes pipes start next2

        farthest = maximum $ Map.unionWith min dists1 dists2

        -- Flood fill both areas. The one not containing (-1,-1) is the pipe enclosed area.
        ((y0,x0),(y1,x1)) = findDefininingPoints (Map.keys input)
        canFill (y,x) = (y,x) `Set.notMember` Map.keysSet dists2
            && y0 - 1 <= y && y <= y1 + 1 && x0 - 1 <= x && x <= x1 + 1

        area1 = floodFill canFill maybeEnclosed1
        area2 = floodFill canFill maybeEnclosed2

        area = Set.size $ if (-1,-1) `Set.member` area1 then area2 else area1

    print farthest
    print area
