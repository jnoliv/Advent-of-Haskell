{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid
import Advent.Utils (readAsMap, count, showMap)
import Data.Bifunctor (first)
import Data.Char (digitToInt, intToDigit)
import Data.Maybe (fromJust)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

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

-- |
-- >> :main
-- 386
-- 2806
main :: IO ()
main = do
    input <- readInputDefaults 2021 15

    let side  = count (== '\n') input
        goal  = (\n -> (n,n)) $ side - 1
        risks = readAsMap (Just . digitToInt) input

    let side'   = 5 * side
        goal'   = (\n -> (n,n)) $ side' - 1
        repeats = [(r, c) | r <- [0..4], c <- [0..4]]
        newRisk r c v = let n = v + r + c in if n <= 9 then n else n `mod` 9
        foldRisk m (r, c) = foldl (\m' ((y,x),v) -> Map.insert (r * side + y, c * side + x) (newRisk r c v) m') m $ Map.toList risks 
        risks'  = foldl foldRisk risks repeats

    print . fromJust $ aStar risks  (0,0) goal
    print . fromJust $ aStar risks' (0,0) goal'

aStar :: Map Coord Int -> Coord -> Coord -> Maybe Int
aStar risks start goal = f (singleton 0 start) Set.empty (Map.singleton start 0)
    where
        get k m = fromJust $ Map.lookup k m

        h = distance goal

        f :: PQueue Coord -> Set Coord -> Map Coord Int -> Maybe Int
        f pQueue seen gScore
            | pQueue == empty = Nothing
            | node   == goal  = Map.lookup node gScore
            | otherwise       = f pQueue'' seen' gScore'
            where
                (node, pQueue') = first (snd . fromJust) $ popMin pQueue
                cost = fromJust $ Map.lookup node gScore

                seen' = Set.insert node seen

                neighbours = filter (\p -> Set.notMember p seen' && Map.member p risks) $ neighbours4 node
                scores     = map (\p -> let c = cost + fromJust (Map.lookup p risks) in (p, c, c + h p)) neighbours

                successors = filter (\(p,c,_) -> Map.notMember p gScore || fromJust (Map.lookup p gScore) > c) scores

                gScore' = foldl (\m (p,c,_) -> Map.insert p c m) gScore successors

                pQueue'' = foldl (\q (p,_,c) -> push c p q) pQueue' successors
