{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid (Coord, distance)
import Advent.Utils (findDefininingPoints, readAsSet, combinations)

import Data.Set (Set)
import Data.Set qualified as Set

expand :: Int -> (Coord,Coord) -> Set Coord -> Set Coord
expand n ((r0,c0),(r1,c1)) galaxies = flip (foldr expandCol) emptyCols $ foldr expandRow galaxies emptyRows
    where
        emptyRows = filter (\r -> (==0) . Set.size $ Set.filter ((==r) . fst) galaxies) [r0..r1]
        emptyCols = filter (\c -> (==0) . Set.size $ Set.filter ((==c) . snd) galaxies) [c0..c1]

        expandRow row = Set.map (\(r,c) -> if r < row then (r,c) else (r+n,c))
        expandCol col = Set.map (\(r,c) -> if c < col then (r,c) else (r,c+n))

pairDistances :: Set Coord -> [Int]
pairDistances = map (\[a,b] -> distance a b) . combinations 2 . Set.elems

-- |
-- >>> :main
-- 9723824
-- 731244261352
main :: IO ()
main = do
    universe <- readAsSet (=='#') <$> readInputDefaults 2023 11

    let bounds    = findDefininingPoints (Set.elems universe)
        expanded  = expand      1 bounds universe
        expanded2 = expand 999999 bounds universe

    print . sum $ pairDistances expanded
    print . sum $ pairDistances expanded2
