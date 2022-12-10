{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec (Parser, readParsed, decimal, sepBy, endBy)
import Advent.Utils (sortDesc)
import Data.List (sortBy)

format :: Parser [[Integer]]
format = group `sepBy` "\n"
    where group = decimal `endBy` "\n"

-- |
-- >>> :main
-- 69693
-- 200945
main :: IO ()
main = do
    caloriesPerElf <- readParsed 2022 1 format

    let totalPerElf       = sortDesc $ map sum caloriesPerElf

    print . head         $ totalPerElf
    print . sum . take 3 $ totalPerElf
