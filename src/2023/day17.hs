{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid (Coord, (.+), right, turnLeft, turnRight, distance)
import Advent.Utils (readAsMap, findDefininingPoints)

--import Control.Applicative
import Control.Lens ((^.), Field1(_1))
import Data.Bifunctor (second)
import Data.Bool (bool)
import Data.Char (digitToInt)
import Data.Maybe (fromJust)

import Data.Map (Map)
import Data.Map qualified as Map

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

popMin :: PQueue a -> (a, PQueue a)
popMin (m:q) = (snd m, q)
popMin     _ = error "can't pop empty queue"

-- position, direction, steps in direction
type Node = (Coord,Coord,Int)

minHeat :: Map Coord Int -> Bool -> Int
minHeat cityMap part = fromJust $ aStar (singleton (distance start goal) startNode) (Map.singleton startNode 0)
    where
        ((r0,c0),(r1,c1)) = findDefininingPoints (Map.keys cityMap)

        goal  = (r1,c1)
        start = (r0,c0)

        startNode = (start, right start, 0)

        aStar :: PQueue Node -> Map Node Int -> Maybe Int
        aStar pQueue gScores
            | null pQueue = Nothing
            | isGoal node = Just (g node)
            | otherwise   = aStar pQueue'' gScores'
            where
                isGoal = (== goal) . (^._1)

                g = (gScores Map.!)
                h = distance goal . (^._1)

                (node, pQueue')   = popMin pQueue
                (pos, dir, steps) = node

                nodeHeat = gScores Map.! node

                -- Neighbour nodes with tentative g scores smaller than previously found.
                nexts  = map (second (+ nodeHeat)) $ neighbours node
                nexts' = filter (\(n,s) -> n `Map.notMember` gScores || s < gScores Map.! n) nexts

                -- Update gScores and priority queue.
                gScores' = foldr (\(n,s) scores -> Map.insert n s scores) gScores nexts'
                pQueue'' = foldr (\(n,s) queue  -> push (s + h n) n queue) pQueue' nexts'

        neighbours = bool neighbours1 neighbours2 part

        neighbours1 :: Node -> [(Node, Int)]
        neighbours1 = map (\n -> (n, getHeat n)) . filter valid . getNeighbours
            where
                getNeighbours = sequence [move turnLeft (const 1), move id (+1), move turnRight (const 1)]

                move f1 f2 (a,b,c) = (a .+ f1 b, f1 b, f2 c)

                valid node@(pos, _, n) = inBounds pos && n <= 3
        
        neighbours2 :: Node -> [(Node, Int)]
        neighbours2 = filter (valid . fst) . getNeighbours
            where
                getNeighbours = sequence [moveSide turnLeft, moveFront, moveSide turnRight]

                moveSide f (pos,dir,_) = ((last steps, newDir, 4), heat)
                    where
                        newDir = f dir
                        steps  = take 4 . tail $ iterate (.+ newDir) pos
                        heat   = sum $ map (\p -> Map.findWithDefault 0 p cityMap) steps
                
                moveFront (a,b,c) = (newNode, getHeat newNode)
                    where
                        newNode = (a .+ b, b, c + 1)

                valid node@(pos, _, n) = inBounds pos && n <= 10

        inBounds (r,c) = r0 <= r && r <= r1 && c0 <= c && c <= c1

        getHeat :: Node -> Int
        getHeat (pos,_,_) = Map.findWithDefault 0 pos cityMap

-- |
-- >>> :main
-- 817
-- 925
main :: IO ()
main = do
    cityMap <- readAsMap (Just . digitToInt) <$> readInputDefaults 2023 17

    print $ minHeat cityMap False
    print $ minHeat cityMap True
