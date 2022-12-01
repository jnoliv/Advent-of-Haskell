{-# LANGUAGE OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec
import Advent.Utils
import Data.List

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

    let totalPerElf       = map sum caloriesPerElf
        sortedTotalPerElf = sortBy (flip compare) totalPerElf

    print $ head sortedTotalPerElf
    print $ sum . take 3 $ sortedTotalPerElf
