{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, BlockArguments #-}

import Advent.Megaparsec (readParsedLines, letterChar, many, Parser)
import Advent.Utils (combinations, sinsert)
import Data.List (intercalate, sort, nub)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

type Adjacencies = Map String (Set String)

parser :: Parser (String,String)
parser = (,) <$> many letterChar <* "-" <*> many letterChar

buildAdjs :: [(String,String)] -> Adjacencies
buildAdjs = foldl adjs Map.empty
    where
        adjs m (a,b) = Map.insertWith Set.union a (Set.singleton b) $
                       Map.insertWith Set.union b (Set.singleton a) m

findCliques :: Int -> Adjacencies -> [[String]]
findCliques n adjs = filter (isClique adjs) maybeCliques
    where
        adjs' = Map.filter ((>= n) . length) adjs
        nodes = Map.keys adjs'

        maybeCliques = combinations n nodes

isClique :: Adjacencies -> [String] -> Bool
isClique adjs nodes = all allConnected nodes
    where
        allConnected node = all (connected node) nodes
        connected    node = (||) <$> (== node) <*> (`Set.member` Map.findWithDefault Set.empty node adjs)

maximalClique :: Adjacencies -> [[String]] -> [String]
maximalClique adjs cliques3 =
        nub . concat . Set.toList .
        snd . last . takeWhile (not . null . snd) $
        iterate f (adjs, cliques3')
    where
        cliques3' :: Set [String]
        cliques3' = Set.fromList $ map sort cliques3

        f :: (Adjacencies, Set [String]) -> (Adjacencies, Set [String])
        f (adjs, cliques) = (adjs', cliques')
            where
                cliques' = Set.fromList [ node `sinsert` clique
                                        | clique <- Set.toList cliques
                                        , node   <- Map.keys adjs
                                        , node `notElem` clique
                                        , let clique' = node `sinsert` clique
                                        , isClique adjs clique']
                toKeep   = Set.fromList . concat $ Set.toList cliques'
                adjs'    = Map.filterWithKey (\k _ -> k `Set.member` toKeep) adjs

password :: [String] -> String
password =  intercalate "," . sort

-- |
-- >>> :main
-- 1156
-- bx,cx,dr,dx,is,jg,km,kt,li,lt,nh,uf,um
main :: IO ()
main = do
    adjs <- buildAdjs <$> readParsedLines 2024 23 parser

    let cliques3  = findCliques 3 adjs

        hasT = any ((== 't') . head)

    print    . length   $ filter        hasT cliques3
    putStrLn . password $ maximalClique adjs cliques3
