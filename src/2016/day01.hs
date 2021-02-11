{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec
import Advent.Coord.Grid
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set

type Instr = (Char, Int)

format :: Parser [Instr]
format = ((,) <$> letterChar <*> decimal) `sepBy` ", "

navigate :: Coord -> Coord -> [Instr] -> [Coord]
navigate  _   _  [] = []
navigate pos dir ((i,n) : instrs) = crossed ++ navigate (last crossed) dir' instrs
    where
        crossed = [pos .+ (x .* dir') | x <- [1..n]]
        dir'    = case i of
            'L' -> turnLeft  dir
            'R' -> turnRight dir

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate l = go Set.empty l
    where
        go _ []          = Nothing
        go seen (x : xs)
            | x `Set.member` seen = Just x
            | otherwise           = go (Set.insert x seen) xs

-- |
-- >>> :main
-- 291
-- 159
main :: IO ()
main = do
    visited <- navigate (0,0) (-1,0) <$> readParsed 2016 1 format

    print . manhattan            $ last           visited
    print . manhattan . fromJust $ firstDuplicate visited

