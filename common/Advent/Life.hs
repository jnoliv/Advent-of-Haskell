module Advent.Life (
    evolve, countAlive
) where

import Advent.Utils (count)
import Data.Set (Set)
import qualified Data.Set as S

-- | Evolve the given grid once. Receives two functions as parameters:
-- 1) neighbours, a function that given a set key, returns a list of all neighbouring keys
-- 2) rules, a function that given a state (False = dead, True = alive) and the number of
-- live neighbours, returns the next state for a grid in such conditions 
evolve :: Ord a => (a -> [a]) -> (Bool -> Int -> Bool) -> Set a -> Set a
evolve neighbours rules set = keep `S.union` new
    where
        allNeighbours  = S.foldr (\v ns -> S.union ns . S.fromList $ neighbours v) S.empty set
        deadNeighbours = allNeighbours S.\\ set

        countLiveNeighbours = count (`S.member` set) . neighbours

        keep = S.filter (rules True  . countLiveNeighbours) set
        new  = S.filter (rules False . countLiveNeighbours) deadNeighbours

-- | Count how many cells are alive in a given grid
countAlive :: Ord a => Set a -> Int
countAlive = S.size
