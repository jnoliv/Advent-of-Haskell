{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, TupleSections, ScopedTypeVariables #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec (Parser, readParsedLines, decimal)
import Advent.Coord.Grid (Coord, neighbours4, origin)
import Data.Composition ((.:))
import Data.Function (on)
import Data.List (minimumBy)
import Data.Maybe (fromJust, isJust, isNothing)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

parser :: Parser Coord
parser = flip (,) <$> decimal <* "," <*> decimal

type Distances a = Map a Int
type Previous  a = Map a [a]

dijkstra :: forall a. (Eq a, Ord a) => (a -> [a]) -> Set a -> a -> a -> Maybe(Int, [[a]])
dijkstra getNeighbours maze start end
    | null costs = Nothing
    | otherwise  = Just (minimum costs, getPaths end result)
    where
        result     = go (Map.singleton start 0) Map.empty (Set.singleton start) Set.empty
        costs = Map.elems . Map.filterWithKey (\k _ -> k == end) $ fst result

        getDist = fromJust .: flip Map.lookup

        go :: Distances a -> Previous a -> Set a -> Set a -> (Distances a, Previous a)
        go dist prev queue seen
            | Set.null queue = (dist, prev)
            | otherwise      = go dist' prev' queue'' seen'
            where
                node   = minimumBy (compare `on` getDist dist) $ Set.toList queue
                queue' = node `Set.delete` queue
                seen'  = node `Set.insert` seen

                filterNeigh = (&&) <$> (`Set.member` maze) <*> (`Set.notMember` seen')

                cost       = getDist dist node
                neighbours = map (, cost + 1) . filter filterNeigh $ getNeighbours node

                withCurrentBest = map (\(pd,c) -> (pd, c, Map.findWithDefault maxBound pd dist)) neighbours
                (dist', prev')  = foldl (updateBest node) (dist, prev) withCurrentBest
                queue''         = foldl (flip Set.insert) queue' $ map fst neighbours

        updateBest :: a -> (Distances a, Previous a) -> (a, Int, Int) -> (Distances a, Previous a)
        updateBest node (dist, prev) (pos, cost, bestCost)
            | cost >  bestCost = (                    dist,                                prev)
            | cost == bestCost = (                    dist, Map.insertWith (++) pos [node] prev)
            | otherwise        = (Map.insert pos cost dist, Map.insert          pos [node] prev)

        getPaths :: a -> (Distances a, Previous a) -> [[a]]
        getPaths end (_, prev) = f [] end
            where
                f :: [a] -> a -> [[a]]
                f path cur
                    | cur == start    = [path']
                    | isNothing nexts = []
                    | otherwise       = concatMap (f path') $ fromJust nexts
                    where
                        nexts = cur `Map.lookup` prev
                        path' = cur : path

-- |
-- >>> :main
-- 264
-- 41,26
main :: IO ()
main = do
    bytes <- readParsedLines 2024 18 parser

    let limit  = 70
        nBytes = 1024

        memSpace = Set.fromList [(y,x) | y <- [0 .. limit], x <- [0 .. limit]]

        afterN         n = foldl (flip Set.delete) memSpace $ take n bytes
        dijkstraAfterN n = dijkstra neighbours4 (afterN n) origin (limit,limit)

        showCoords (y,x) = show x ++ "," ++ show y

    print . fst . fromJust $ dijkstraAfterN nBytes
    putStrLn . showCoords . (bytes !!) . pred . head $ dropWhile (isJust . dijkstraAfterN) [1 .. length bytes - 1]
