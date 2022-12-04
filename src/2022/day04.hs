{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec (Parser, readParsedLines, decimal)
import Advent.Utils (count)

data Range = Range Integer Integer

format :: Parser (Range, Range)
format = (,) <$> range <* "," <*> range
    where range = Range <$> decimal <* "-" <*> decimal

contains :: Range -> Range -> Bool
contains (Range s1 e1) (Range s2 e2) = s1 <= s2 && e2 <= e1

oneIsContained :: Range -> Range -> Bool
oneIsContained r1 r2 = r1 `contains` r2 || r2 `contains` r1

overlaps :: Range -> Range -> Bool
overlaps (Range s1 e1) (Range s2 e2) =
    (s2 <= s1 && s1 <= e2) || (s2 <= e1 && e1 <= e2)

oneOverlaps :: Range -> Range -> Bool
oneOverlaps r1 r2 = r1 `overlaps` r2 || r2 `overlaps` r1

-- |
-- >>> :main
-- 500
-- 815
main :: IO ()
main = do
    input <- readParsedLines 2022 4 format

    let numContainedPairs   = count (uncurry oneIsContained) input
        numOverlappingPairs = count (uncurry oneOverlaps   ) input

    print numContainedPairs
    print numOverlappingPairs

