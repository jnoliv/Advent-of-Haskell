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

--import Data.List.Split
--
--import Data.Map (Map)
--import Data.Map qualified as Map
--import Data.Set (Set)
--import Data.Set qualified as Set

-- Move n from x to y
data Move = Move Int Int Int
    deriving Show

format :: Parser ([[Char]], [Move])
format = (,) <$> formatStacks <* "\n" <*> (formatMove `endBy` "\n")

formatStacks :: Parser [[Char]]
formatStacks = reformat <$> (box `sepBy` " ") `endBy` "\n"
    where
        box = try ("   " $> ' ')
           <|>    ("[" *> letterChar <* "]")
           <|>    (" " *> asciiChar  <* " ")
        
        -- Remove empty spaces and drop last line
        reformat :: [[Char]] -> [[Char]]
        reformat = map (filter (/=' ')) . init

formatMove :: Parser Move
formatMove = Move <$> ("move " *> decimal) <* " from " <*> stackNumber <* " to " <*> stackNumber
    where stackNumber = (\x -> x-1) <$> decimal -- stack indexes are given 1-indexed

doMove :: ([Char] -> [Char]) -> [[Char]] -> Move -> [[Char]]
doMove f stacks (Move n x y) = setStack y newStackAtY . setStack x newStackAtX $ stacks
    where
        stackAtX              = stacks !! x
        stackAty              = stacks !! y
        (toMove, newStackAtX) = splitAt n stackAtX
        newStackAtY           = f toMove ++ stackAty

setStack :: Int -> [Char] -> [[Char]] -> [[Char]]
setStack n s stacks = take n stacks ++ [s] ++ drop (n + 1) stacks

-- |
-- >>> :main
-- SHMSDGZVC
main :: IO ()
main = do
    {-
        [G]                 [D] [R]        
        [W]         [V]     [C] [T] [M]    
        [L]         [P] [Z] [Q] [F] [V]    
        [J]         [S] [D] [J] [M] [T] [V]
        [B]     [M] [H] [L] [Z] [J] [B] [S]
        [R] [C] [T] [C] [T] [R] [D] [R] [D]
        [T] [W] [Z] [T] [P] [B] [B] [H] [P]
        [D] [S] [R] [D] [G] [F] [S] [L] [Q]
         1   2   3   4   5   6   7   8   9 
    -}

    --(stacks', moves') <- readParsed 2022 5 format
    --print stack'

    -- TODO: fix parsing of initial stacks
    let stacks =
            [ ['G','W','L','J','B','R','T','D']
            , ['C','W','S']
            , ['M','T','Z','R']
            , ['V','P','S','H','C','T','D']
            , ['Z','D','L','T','P','G']
            , ['D','C','Q','J','Z','R','B','F']
            , ['R','T','F','M','J','D','B','S']
            , ['M','V','T','B','R','H','L']
            , ['V','S','D','P','Q'] ]

    moves <- readParsedLines 2022 5 formatMove

    let final1 = foldl (doMove reverse) stacks moves
        tops1  = map head final1

        final2 = foldl (doMove id) stacks moves
        tops2  = map head final2

    putStr (tops1 ++ "\n")
    putStr (tops2 ++ "\n")
