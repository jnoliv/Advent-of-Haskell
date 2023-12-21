{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid (neighbours4, Coord)
import Advent.Utils (readAsMap)

import Control.Applicative (liftA2, empty)
import Data.Bifunctor (bimap)
import Data.Bool (bool)
import Data.List (find)
import Data.Maybe (fromJust)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import GHC.Float (int2Double, double2Int)
import Math.Regression.Simple (quadratic, V3(V3))

step :: (Set Coord -> Coord -> Bool) -> Set Coord -> Set Coord -> Set Coord
step f garden = Set.fromList . filter (f garden) . concatMap neighbours4 . Set.elems

cells :: Set Coord -> Coord -> Bool
cells garden = (`Set.member` garden) . bimap (`mod` 131) (`mod` 131)

-- |
-- >>> :main
-- 3737
-- 625382480005896
main :: IO ()
main = do
    input <- readInputDefaults 2023 21

    let asMap = readAsMap (liftA2 (bool empty) pure (/= '#')) input
        
        start  = fst . fromJust . find ((== 'S') . snd) $ Map.toList asMap
        garden = Map.keysSet asMap

        steps  = iterate (step cells garden) (Set.singleton start)

    print . Set.size $ steps !! 64

    let numSteps = 26501365
        size     = length (lines input)

        f i = Set.size $ steps !! (fst start + i * size)

        (V3 a b c) = quadratic id [bimap int2Double int2Double (i, f i ) | i <- [0..2]]

        a' =  double2Int a
        b' =  double2Int b
        c' =  double2Int c

        g x = a' * x^2 + b' * x + c'
        n   = numSteps `div` size

    print (g n)
