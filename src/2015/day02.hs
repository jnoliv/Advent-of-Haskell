{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec
import Data.List (sort, tails)

format :: Parser [Int]
format = decimal `sepBy` "x"

paper :: [Int] -> Int
paper dim = slack + sum [2 * x * y | (x:ys) <- tails dim, y <- ys]
    where slack = product . take 2 $ sort dim

-- |
-- >>> :main
-- 1606483
main :: IO ()
main = do
    input <- readParsedLines 2015 2 format

    print . sum . map paper $ input
