{-# LANGUAGE OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec (readParsedLines, decimal, sepBy, Parser)
import Advent.Utils (count)

parser :: Parser [Int]
parser = decimal `sepBy` " "

safe :: [Int] -> Bool
safe report
    | head diffs > 0 = all (\n -> 1 <= n && n <= 3)   diffs
    | head diffs < 0 = all (\n -> -3 <= n && n <= -1) diffs
    | otherwise      = False
    where
        diffs = zipWith (-) report (tail report)

sublists :: [Int] -> [[Int]]
sublists l = [take n l ++ drop (n+1) l | n <- [0 .. length l]]

-- |
-- >>> :main
-- 202
-- 271
main :: IO ()
main = do
    input <- readParsedLines 2024 2 parser

    print $ count safe             input
    print $ count (any safe . sublists) input
