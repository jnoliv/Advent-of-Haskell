{-# Language ImportQualifiedPost, TupleSections #-}

module Advent.Graph (
    aStar
) where

import Advent.Coord.Grid
import Advent.Utils (readAsMap, count, showMap)
import Data.Bifunctor (first)
import Data.Maybe (fromJust)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Debug.Trace

-- Naive priority queue

type PQueue a = [(Int, a)]

empty :: PQueue a
empty = []

singleton :: Int -> a -> PQueue a
singleton w a = push w a empty

push :: Int -> a -> PQueue a -> PQueue a
push w a []             = [(w,a)]
push w a ((w',a') : qs)
    | w < w'    = (w,a) : (w',a') : qs
    | otherwise = (w',a') : push w a qs

popMin :: PQueue a -> (Maybe (Int, a), PQueue a)
popMin []    = (Nothing, [])
popMin (m:q) = (Just m, q)

-- Graph functions

-- | Computes the shortest path, in the given map, from one of the start points to the end point.
-- Returns Nothing if no path exists, Just <shortest_path_length> otherwise
aStar ::
    -- TODO: parameterize g, h, and neighbours
    --       support multiple end points
    --       return shortest path instead (more generic, but length can still be easily determined)
    --       use a better queue structure
    (Map Coord Int -> Coord -> [Coord] -> [Coord]) -- filter to apply to possible neighbours of current node
    -> Map Coord Int                               -- 2D map where search is happening
    -> [Coord]                                     -- list of start points
    -> Coord                                       -- end point
    -> Maybe Int                                   -- Nothing if no path found, otherwise path length wrapped in Just
aStar filterNeighbours inputMap starts goal = f (map (0,) starts) Set.empty (Map.fromList $ map (,0) starts)
    where
        get k m = fromJust $ Map.lookup k m

        h = distance goal

        f :: PQueue Coord -> Set Coord -> Map Coord Int -> Maybe Int
        f pQueue seen gScore
            | pQueue == empty = Nothing
            | node   == goal  = Map.lookup node gScore
            | otherwise       = --trace ("cur: " ++ show node ++ ", G: " ++ show (fromJust $ Map.lookup node gScore)) $
                f pQueue'' seen' gScore'
            where
                (node, pQueue') = first (snd . fromJust) $ popMin pQueue
                cost = fromJust $ Map.lookup node gScore

                seen' = Set.insert node seen

                neighbours = filterNeighbours inputMap node . filter (\p -> Map.member p inputMap && Set.notMember p seen') $ neighbours4 node
                scores     = map (\p -> let c = cost + 1 in (p, c, c + h p)) neighbours

                successors = filter (\(p,c,_) -> Map.notMember p gScore || fromJust (Map.lookup p gScore) > c) scores

                gScore' = foldl (\m (p,c,_) -> Map.insert p c m) gScore successors

                pQueue'' = foldl (\q (p,_,c) -> push c p q) pQueue' successors
