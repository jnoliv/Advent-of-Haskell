{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid ( Coord, neighbours4 )
import Advent.Utils ( readAsMap )

import Data.Char ( digitToInt )
import Data.List ( sort )
import Data.Maybe ( mapMaybe )

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set


-- |
-- >>> :main
-- 417
-- 1148965
main :: IO ()
main = do
    heightmap <- readAsMap (Just . digitToInt) <$> readInputDefaults 2021 9

    let lows = lowPoints heightmap

    print $ totalRiskLevel heightmap lows

    let basinSizes = map (basinSize heightmap) lows

    print . product . take 3 . reverse . sort $ basinSizes

-- | Finds all the low points in the heightmap, i.e., all points
-- whose neighbours are all higher.
lowPoints :: Map Coord Int -> [Coord]
lowPoints m = Map.foldlWithKey f [] m
    where
        f acc k v
            | any (<= v) heights = acc
            | otherwise          = k : acc
            where
                heights = mapMaybe (`Map.lookup` m) $ neighbours4 k

-- | Calculates the sum of all low points risk levels, i.e., the
-- height of the low point plus 1.
totalRiskLevel :: Map Coord Int -> [Coord] -> Int
totalRiskLevel m = sum . map (+ 1) . mapMaybe (`Map.lookup` m)

-- | Calculates the size of the basin containing the given low
-- point. Does so by repeatedly adding to the current basin points
-- all neighbours that are also in the basin, until the entire basin
-- has been found.
basinSize :: Map Coord Int -> Coord -> Int
basinSize m k = Set.size $ f (Set.singleton k)
    where
        f set
            | Set.size newBasinPoints == 0 = set
            | otherwise                    = f (Set.union set newBasinPoints)
            where
                isNew k = Set.notMember k set && Map.findWithDefault 9 k m /= 9

                allNeighbours  = Set.fromList . concatMap neighbours4 . Set.toList $ set
                newBasinPoints = Set.filter isNew allNeighbours
