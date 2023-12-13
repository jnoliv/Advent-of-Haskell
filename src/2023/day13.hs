{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid (Coord)
import Advent.Utils (readAsSet)

import Data.Bifunctor (first, second)
import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import Data.Set qualified as Set

data Axis = Row Int | Col Int

toInt :: Axis -> Int
toInt (Row n) = 100 * (n + 1)
toInt (Col n) = n + 1

findReflection :: (Set Coord -> Set Coord -> Bool) -> Set Coord -> (Int, Int) -> Axis
findReflection f grid (r,c)
    | isJust horizontal = Row (fromJust horizontal)
    | isJust vertical   = Col (fromJust vertical)
    | otherwise         = error "no reflection found"
    where
        horizontal = True `elemIndex` [isMirroredH f grid r n | n <- [0 .. r - 2]]
        vertical   = True `elemIndex` [isMirroredV f grid c n | n <- [0 .. c - 2]]

isMirroredH, isMirroredV :: (Set Coord -> Set Coord -> Bool) -> Set Coord -> Int -> Int -> Bool
isMirroredH = isMirrored (fst, first)
isMirroredV = isMirrored (snd, second)

isMirrored (get, set) f grid size axis = f left'' right'
    where
        n = min (axis + 1) (size - axis - 1)

        (left, right) = Set.partition ((<= axis) . get) grid

        left'  = Set.filter ((>  (axis - n)) . get) left
        right' = Set.filter ((<= (axis + n)) . get) right

        left'' = Set.map (set (\c -> axis + (axis - c) + 1)) left'

isSmudge :: Set Coord -> Set Coord -> Bool
isSmudge s1 s2 = min n1 n2 == 0 && max n1 n2 == 1
    where
        n1 = Set.size (s1 Set.\\ s2)
        n2 = Set.size (s2 Set.\\ s1)

-- |
-- >>> :main
-- 34911
-- 33183
main :: IO ()
main = do
    rawGrids <- splitOn "\n\n" <$> readInputDefaults 2023 13

    let sizes = map ((\ls -> (length ls, length (head ls))) . lines) rawGrids
        grids = map (readAsSet (== '#')) rawGrids

        gridsWithSize = zip grids sizes

        summarise f = sum . map (toInt . uncurry (findReflection f))

    print $ summarise (==)     gridsWithSize
    print $ summarise isSmudge gridsWithSize
