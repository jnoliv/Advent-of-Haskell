{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid (Coord, origin, turnRight, up, (.+))
import Advent.Utils (count, mapDimensions, readAsMap)
import Data.Maybe (isJust)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

patrol :: (Int,Int) -> Set Coord -> Set (Coord,Coord) -> (Coord,Coord) -> Maybe (Set Coord)
patrol dims@(lx,ly) obstrs seen p@(pos, dir)
    | p `Set.member` seen = Nothing         -- loop
    | not (inside pos)    = Just seenPos    -- out of bounds
    | otherwise           = patrol dims obstrs (p `Set.insert` seen) (walk obstrs p)
    where
        inside (x,y) = 0 <= x && x < lx
                    && 0 <= y && y < ly
        
        seenPos = Set.fromList . map fst . Set.toList $ seen

walk :: Set Coord -> (Coord, Coord) -> (Coord, Coord)
walk obstrs (pos, dir)
    | next `Set.member` obstrs = (pos, newDir)
    | otherwise                = (next, dir)
    where
        next   = pos .+ dir
        newDir = turnRight dir

loops :: (Int,Int) -> Set Coord -> (Coord,Coord) -> Coord -> Bool
loops dims obstrs p newObstr
    | isJust res = False
    | otherwise  = True
    where
        res = patrol dims (newObstr `Set.insert` obstrs) Set.empty p

-- |
-- >>> :main
-- 5212
-- 1767
main :: IO ()
main = do
    input <- readInputDefaults 2024 6

    let grid = readAsMap Just input

        obstrs = Set.fromList . map fst . filter ((== '#') . snd) . Map.toList $ grid
        start  = fst . head . filter ((== '^') . snd) . Map.toList $ grid
        dims   = mapDimensions grid

        Just path = patrol dims obstrs Set.empty (start, up origin)

    print (Set.size path)
    print . count (loops dims obstrs (start, up origin)) . Set.toList $ start `Set.delete` path
