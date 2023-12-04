{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, BlockArguments, TupleSections #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec
import Advent.Coord.Grid
import Advent.Life
import Advent.Math
import Advent.Utils

import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe

--import Data.List.Index
--import Data.List.Split

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Debug.Trace

format :: Parser ([Int], [Int])
format = parseId *> parseNums
    where
        parseId = trim ("Card" *> some " ") decimal ":" <* some " "
        parseNums = (,)
                <$> decimal `endBy` some " "
                <*  "|" <* some " "
                <*> decimal `sepBy` some " "


copies :: [Int] -> [Int]
copies points = copies' points $ replicate (length points) 1
    where
        copies' (n : ns) (c : cs) = c : copies' ns (nmap (+c) n cs)
        copies' _ _ = []

        nmap f 0 l  = l
        nmap f _ [] = []
        nmap f n (h : t) = f h : nmap f (n - 1) t

-- |
-- >>> :main
-- 27845
-- 9496801
main :: IO ()
main = do
    input <- readParsedLines 2023 4 format

    let wins   = map (length . uncurry intersect) input
        points = map ((2^) . max 0) wins

    print $ sum points
    print $ sum (copies wins)
