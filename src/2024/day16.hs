{-# LANGUAGE ImportQualifiedPost, BlockArguments, TupleSections #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid ((.+), down, left, origin, right, turnLeft, turnRight, up, Coord)
import Advent.Utils (readAsMap)

import Control.Applicative (liftA2, empty)
import Data.Bool (bool)
import Data.Composition ((.:))
import Data.Function (on)
import Data.List (minimumBy)
import Data.Maybe (fromJust, isNothing)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

type PosDir    = (Coord,Coord)
type Distances = Map PosDir Int
type Previous  = Map PosDir [PosDir]

dijkstra :: Set Coord -> PosDir -> Coord -> (Int, [[Coord]])
dijkstra maze start end = (cost, getPaths end result)
    where
        result = go (Map.singleton start 0) Map.empty (Set.singleton start) Set.empty
        cost   = minimum . Map.elems . Map.filterWithKey (\k _ -> fst k == end) $ fst result

        getDist = fromJust .: flip Map.lookup

        go :: Distances -> Previous -> Set PosDir -> Set PosDir -> (Distances,Previous)
        go dist prev queue seen
            | Set.null queue = (dist, prev)
            | otherwise      = go dist' prev' queue'' seen'
            where
                node@(pos,dir) = minimumBy (compare `on` getDist dist) $ Set.toList queue
                queue'         = node `Set.delete` queue
                seen'          = node `Set.insert` seen

                filterNeigh = (&&) <$> ((`Set.member` maze) . fst) <*> (`Set.notMember` seen')

                cost       = getDist dist node
                neighbours = filter (filterNeigh . fst) [
                        ((pos .+ dir,           dir), cost + 1   ),
                        ((pos       , turnLeft  dir), cost + 1000),
                        ((pos       , turnRight dir), cost + 1000)
                    ]

                withCurrentBest = map (\(pd,c) -> (pd, c, Map.findWithDefault maxBound pd dist)) neighbours
                (dist', prev')  = foldl (updateBest node) (dist, prev) withCurrentBest
                queue''         = foldl (flip Set.insert) queue' $ map fst neighbours

        updateBest :: PosDir -> (Distances,Previous) -> (PosDir,Int,Int) -> (Distances,Previous)
        updateBest node (dist, prev) (posDir, cost, bestCost)
            | cost >  bestCost = (                       dist,                                   prev)
            | cost == bestCost = (                       dist, Map.insertWith (++) posDir [node] prev)
            | otherwise        = (Map.insert posDir cost dist, Map.insert          posDir [node] prev)

        getPaths :: Coord -> (Distances,Previous) -> [[Coord]]
        getPaths end (_, prev) = concatMap (f []) ends
            where
                ends = map (end,) $ sequence [up, right, down, left] origin

                f :: [Coord] -> PosDir -> [[Coord]]
                f path cur
                    | cur == start    = [path']
                    | isNothing nexts = []
                    | otherwise       = concatMap (f path') $ fromJust nexts
                    where
                        nexts = cur `Map.lookup` prev
                        path' = fst cur : path

-- |
-- >>> :main
-- 73432
-- 496
main :: IO ()
main = do
    input <- readAsMap (liftA2 (bool empty) pure (/= '#')) <$> readInputDefaults 2024 16

    let start = head . Map.keys $ Map.filter (== 'S') input
        end   = head . Map.keys $ Map.filter (== 'E') input

        maze  = Set.fromList (Map.keys input)
        east  = right origin

        (cost, paths) = dijkstra maze (start, east) end

    print cost
    print . Set.size . Set.fromList . concat $ paths
