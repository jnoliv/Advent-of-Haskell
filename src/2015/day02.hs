{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec
import Data.List (sort, tails)

format :: Parser [Int]
format = decimal `sepBy` "x"

paper :: [Int] -> Int
paper dim = slack + sum [2 * x * y | (x:ys) <- tails dim, y <- ys]
    where slack = product . take 2 $ dim

ribbon :: [Int] -> Int
ribbon dim = bow + (sum . map (*2) . take 2 $ dim)
    where bow = product dim

-- |
-- >>> :main
-- 1606483
-- 3842356
main :: IO ()
main = do
    input <- map sort <$> readParsedLines 2015 2 format

    print . sum . map paper  $ input
    print . sum . map ribbon $ input
