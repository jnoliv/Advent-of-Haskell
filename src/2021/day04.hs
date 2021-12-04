{-# LANGUAGE OverloadedStrings, TupleSections #-}

import Advent.Megaparsec
import Data.List (transpose, partition)

type Board = [[(Int, Bool)]]

-- | This one took a while, good luck reading it.
format :: Parser ([Int], [Board])
format = (,) <$> numbers <* "\n\n" <*> (board `sepBy` "\n")
    where
        numbers = decimal `sepBy` ","
        board   = line `endBy1` "\n"
        line    = ((, False) <$> number) `sepBy1` some " "
        number  = optional (many " ") *> decimal

-- |
-- >>> :main
-- 38594
-- 21184
main :: IO ()
main = do
    (numbers, boards) <- readParsed 2021 4 format

    let scores = play boards numbers
    
    print $ head scores
    print $ last scores

-- | Returns a list of board scores in the order they win.
play :: [[[(Int, Bool)]]] -> [Int] -> [Int]
play [] _ = []
play boards (n:ns) = 
    map ((*) n . score) winners ++ play losers ns
    where
        markedBoards      = map (mark n) boards
        (winners, losers) = partition isWinner markedBoards


-- | Marks the number n in the given board.
-- >>> mark 3 [[(1,False), (2,True)], [(3, False), (4, False)]]
-- [[(1,False),(2,True)],[(3,True),(4,False)]]
mark :: Int -> Board -> Board
mark n = map (map markNumber)
    where
        markNumber (x,m) = if x == n then (x, True) else (x,m) 

-- | Determines whether the board has a fully marked row or column.
-- >>> isWinner [[(1,False), (2,True)], [(3, False), (4, True)]]
-- True
--
-- >>> isWinner [[(1,True), (2,True)], [(3, False), (4, False)]]
-- True
--
-- >>> isWinner [[(1,False), (2,True)], [(3, False), (4, False)]]
-- False
isWinner :: [[(a, Bool)]] -> Bool
isWinner board = f board || f (transpose board)
    where
        f = any (all snd)

-- | Filters all unmarked numbers and returns their sum.
-- >>> score [[(1,False), (2,True)], [(3, True), (4, False)]]
-- 5
--
-- >>> score [[(1,True), (2,True)], [(3, False), (4, False)]]
-- 7
score :: Board -> Int
score = sum . map fst . filter (not . snd) . concat
