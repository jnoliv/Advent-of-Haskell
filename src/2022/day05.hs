{-# LANGUAGE OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec
import Data.List (transpose)

-- Move n from x to y
data Move = Move Int Int Int
    deriving Show

format :: Parser ([[Char]], [Move])
format = (,) <$> formatStacks <*> (formatMove `endBy` "\n")

formatStacks :: Parser [[Char]]
formatStacks = reformat <$> manyTill boxes "\n"
    where
        boxes = (box `sepBy1` " ") <* "\n"
        box = try ("   " $> ' ')
          <|>     ("[" *> letterChar <* "]")
          <|>     (" " *> asciiChar  <* " ")
        
        -- Drop last line of indexes, transpose the values, then remove empty spaces.
        reformat = map (filter (/=' ')) . transpose . init

formatMove :: Parser Move
formatMove = Move <$> ("move " *> decimal) <* " from " <*> stackNumber <* " to " <*> stackNumber
    where stackNumber = (flip (-) 1) <$> decimal -- stack indexes are given 1-indexed

-- Actual non-parsing code starts here

doMove :: ([Char] -> [Char]) -> [[Char]] -> Move -> [[Char]]
doMove f stacks (Move n x y) = setStack y newStackAtY . setStack x newStackAtX $ stacks
    where
        stackAtX              = stacks !! x
        stackAtY              = stacks !! y
        (toMove, newStackAtX) = splitAt n stackAtX
        newStackAtY           = f toMove ++ stackAtY

setStack :: Int -> [Char] -> [[Char]] -> [[Char]]
setStack n s stacks = take n stacks ++ [s] ++ drop (n + 1) stacks

-- |
-- >>> :main
-- SHMSDGZVC
-- VRZGHDFBQ
main :: IO ()
main = do
    (stacks, moves) <- readParsed 2022 5 format

    let final1 = foldl (doMove reverse) stacks moves
        tops1  = map head final1

        final2 = foldl (doMove id) stacks moves
        tops2  = map head final2

    putStrLn tops1
    putStrLn tops2
