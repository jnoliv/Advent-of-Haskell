{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid ((.+), down, left, origin, right, turnLeft, turnRight, up, Coord)
import Advent.Utils (findDefininingPoints, readAsMap)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

countEnergized :: Map Coord Char -> Coord -> Coord -> Int
countEnergized contraption pos dir = Set.size . Set.map fst $ traceBeam Set.empty pos dir
    where
        traceBeam :: Set (Coord,Coord) -> Coord -> Coord -> Set (Coord, Coord)
        traceBeam seen pos dir
            | tile == '#'      = seen
            | isHorizontal dir = case tile of
                '/'  -> move seen  pos (turnLeft  dir)
                '\\' -> move seen  pos (turnRight dir)
                '|'  -> move seen' pos (turnLeft  dir) where seen' = move seen pos (turnRight dir)
                _    -> move seen  pos dir
            | isVertical dir = case tile of
                '/'  -> move seen  pos (turnRight dir)
                '\\' -> move seen  pos (turnLeft  dir)
                '-'  -> move seen' pos (turnRight dir) where seen' = move seen pos (turnLeft dir)
                _    -> move seen  pos dir
            | otherwise    = error "invalid direction"
            where
                tile = Map.findWithDefault '#' pos contraption

                isHorizontal dir = dir == left origin || dir == right origin
                isVertical   dir = dir == up   origin || dir == down  origin

        move :: Set (Coord,Coord) -> Coord -> Coord -> Set (Coord, Coord)
        move seen pos dir
            | (pos, dir) `Set.member` seen = seen
            | otherwise                    = traceBeam (Set.insert (pos,dir) seen) (pos .+ dir) dir

maxEnergized :: Map Coord Char -> Int
maxEnergized contraption = maximum . map maximum $ [downward, upward, leftward, rightward]
    where
        ((r0,c0),(r1,c1)) = findDefininingPoints $ Map.keys contraption

        downward  = [countEnergized contraption (r0,c) (down  origin) | c <- [c0..c1]]
        upward    = [countEnergized contraption (r1,c) (up    origin) | c <- [c0..c1]]
        leftward  = [countEnergized contraption (r,c1) (left  origin) | r <- [r0..r1]]
        rightward = [countEnergized contraption (r,c0) (right origin) | r <- [r0..r1]]

-- |
-- >>> :main
-- 7199
-- 7438
main :: IO ()
main = do
    contraption <- readAsMap Just <$> readInputDefaults 2023 16

    print $ countEnergized contraption origin (right origin)
    print $   maxEnergized contraption
