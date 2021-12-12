{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec (letterChar, some, Parser, readParsedLines)

import Data.Bifunctor (second)
import Data.Char (isLower)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)

type Cave = String
type Adj  = Map Cave [Cave]
type Seen = Map Cave Int

format :: Parser (String, String)
format = (,) <$> node <* "-" <*> node
    where node = some letterChar

-- |
-- >>> :main
-- 3292
-- 89592
main :: IO ()
main = do
    input <- readParsedLines 2021 12 format

    let allConns  = input ++ map swap input
        allConns' = filter ((/= "start") . snd) allConns
        adj       = Map.fromListWith (++) $ map (second (: [])) allConns'
    
    print $ countPaths canVisit      adj Map.empty "start"
    print $ countPaths canVisitTwice adj Map.empty "start"

-- | Counts all possible paths from cur to "end", where adj is the
-- adjacency list of the graph, seen is a map of nodes to how many
-- times they have been visited and f is the filter that decides
-- whether a node can be visited or not.
countPaths :: (Seen -> Cave -> Bool) -> Adj -> Seen -> Cave -> Int
countPaths f adj seen cur
    | cur == "end" = 1
    | otherwise    = sum $ map (countPaths f adj seen') neighs
    where
        seen'  = visit seen cur
        neighs = filter (f seen') $ Map.findWithDefault [] cur adj

-- | Marks the cave as seen, if it is a small cave.
visit :: Seen -> Cave -> Seen
visit seen c
    | isLower (head c) = Map.insertWith (+) c 1 seen
    | otherwise        = seen

-- | Allows visiting only once.
canVisit :: Seen -> Cave -> Bool
canVisit seen cave = Map.notMember cave seen

-- | Allows visiting a single cave twice and the rest once.
canVisitTwice :: Seen -> Cave -> Bool
canVisitTwice seen cave
    | twice     = Map.notMember cave seen
    | otherwise = True
    where
        twice = any ((>= 2) . snd) $ Map.toList seen
