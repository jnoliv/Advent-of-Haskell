{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec
import Advent.Math (powMod)

format :: Parser (Int,Int)
format = (,) <$> ("To continue, please consult the code grid in the manual.  Enter the code at row " *> decimal)
             <*> (", column " *> decimal <* ".")

-- | The number of iterations i from (1,1) to (r,c) is equal to
-- i(r',1) + (c - 1) where r' = r + c - 1. In other words, the
-- number of iterations to a given position is equal to the number
-- of iterations to the first position in the current diagonal plus
-- the number of iterations from that first position to the given
-- position.
-- The iterations to (r',1) is `sum [1 .. r' - 1]` or, better yet,
-- `r' * (r' - 1) `div` 2`, as each diagonal starting at row n has
-- n elements in it.
--
-- >>> iterations (5,1)
-- 10
--
-- >>> iterations (4,3)
-- 17
-- 
-- >>> iterations (2,2)
-- 4
--
-- >>> iterations (1,1)
-- 0
iterations :: (Int, Int) -> Int
iterations (r,c) = (r' * (r' - 1) `div` 2) + c'
    where
        c' = c - 1
        r' = r + c'

genCode :: (Int,Int) -> Int
genCode pos = 20151125 * n `mod` 33554393
    where
        n = powMod 252533 (iterations pos) 33554393

-- |
-- >>> :main
-- 8997277
main :: IO ()
main = print =<< genCode <$> readParsed 2015 25 format
