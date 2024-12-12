{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid (col, neighbours4, row, Coord)
import Advent.Utils (count, mapDimensions, readAsMap)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

findRegions :: Map Coord Char -> [Set Coord]
findRegions plots = snd . foldl f (Set.empty, []) . Map.keys $ plots
    where
        f (seen, regions) plot
            | plot `Set.member` seen = (seen, regions)
            | otherwise              = (Set.union region seen, region : regions)
            where
                region = flood plots plot

flood :: Map Coord Char -> Coord -> Set Coord
flood plots start = go Set.empty start
    where
        Just plant = start `Map.lookup` plots

        canFill = (== plant) . flip (Map.findWithDefault ' ') plots

        go seen pos
            | null nexts = seen'
            | otherwise  = foldl go seen' nexts
            where
                seen' = pos `Set.insert` seen
                nexts = filter ((&&) <$> (`Set.notMember` seen') <*> canFill) $ neighbours4 pos

perimeter :: Map Coord Char -> Set Coord -> Int
perimeter plots plot = sum . map (length . eqNeighbours) $ Set.elems plot
    where
        getPlant = flip (Map.findWithDefault ' ') plots
        plant    = getPlant $ head (Set.elems plot)

        eqNeighbours = filter (/= plant) . map getPlant . neighbours4

area :: Set Coord -> Int
area = Set.size

sides :: Map Coord Char -> Set Coord -> Int
sides plots plot = fst (foldl (sweep row col) empty rows) + fst (foldl (sweep col row) empty cols)
    where
        empty = (0, Set.empty)

        (ly, lx) = mapDimensions plots

        rows = [0 .. ly]
        cols = [0 .. lx]

        sweep :: (Coord -> Int) -> (Coord -> Int) -> (Int, Set Int) -> Int -> (Int, Set Int)
        sweep get1 get2 (cnt, open) cur = (cnt', open'')
            where
                cnt'   = cnt + countSides opening + countSides closing
                open'  = foldl (flip Set.insert) open  opening
                open'' = foldl (flip Set.delete) open' closing

                inLine = map get2 .filter ((== cur) . get1) $ Set.elems plot

                countSides [] = 0
                countSides  l = succ . count (uncurry ((/=) . succ <$> id)) $ zip l (tail l)

                opening = filter (`Set.notMember` open) inLine
                closing = filter (`notElem` inLine) (Set.elems open)

-- |
-- >>> :main
-- 1450422
-- 906606
main :: IO ()
main = do
    input <- readAsMap Just <$> readInputDefaults 2024 12

    print . sum . map ((*) <$> perimeter input <*> area) . findRegions $ input
    print . sum . map ((*) <$> sides     input <*> area) . findRegions $ input
