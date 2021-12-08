{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec (readParsedLines, letterChar, some, sepBy, endBy, Parser)
import Advent.Utils (count)
import Data.List (find, (\\), elemIndex, findIndex, intersect, sort)
import Data.Maybe (fromJust)

format :: Parser ([String], [String])
format = (,) <$> patterns <* "| " <*> outputs
    where
        patterns = (sort <$> some letterChar) `endBy` " "
        outputs  = (sort <$> some letterChar) `sepBy` " "

-- |
-- >>> :main
-- 504
-- 1073431
main :: IO ()
main = do
    input <- readParsedLines 2021 8 format

    print . count1478 . concatMap snd $ input
    print . sum . map decode $ input

-- | Counts the occurrence of the digits 1, 4, 7, and 8. Since these digits have
-- a unique number of turned on segments, we simply filter the elements with that
-- amount of segments and count how many remain. As seen below, the amount of
-- segments in 1, 4, 7 and 8 is, respectively, 2, 4, 3 and 7.
--   1:      7:      4:      8:  
--  ....    aaaa    ....    aaaa 
-- .    c  .    c  b    c  b    c
-- .    c  .    c  b    c  b    c
--  ....    ....    dddd    dddd 
-- .    f  .    f  .    f  e    f
-- .    f  .    f  .    f  e    f
--  ....    ....    ....    gggg 
--
-- >>> count1478 ["fdgacbe","cefdb","cefbgd","gcbe","fcgedb","cgb","dgebacf","gc"]
-- 5
count1478 :: [String] -> Int
count1478 = length . filter ((`elem` [2,4,3,7]) . length)

-- | Decodes the numbers from all unique patterns, returning the 4 digit integer
-- the numbers represent. Requires the letters in each pattern to be sorted.
-- This representation of all numbers will help understand the comments in the code.
--   0:      1:      2:      3:      4:
--  aaaa    ....    aaaa    aaaa    ....
-- b    c  .    c  .    c  .    c  b    c
-- b    c  .    c  .    c  .    c  b    c
--  ....    ....    dddd    dddd    dddd
-- e    f  .    f  e    .  .    f  .    f
-- e    f  .    f  e    .  .    f  .    f
--  gggg    ....    gggg    gggg    ....
-- 
--   5:      6:      7:      8:      9:
--  aaaa    aaaa    aaaa    aaaa    aaaa
-- b    .  b    .  .    c  b    c  b    c
-- b    .  b    .  .    c  b    c  b    c
--  dddd    dddd    ....    dddd    dddd
-- .    f  e    f  .    f  e    f  .    f
-- .    f  e    f  .    f  e    f  .    f
--  gggg    gggg    ....    gggg    gggg
-- >>> decode (["abcdefg","bcdef","acdfg","abcdf","abd","abcdef","bcdefg","abef","abcdeg","ab"], ["bcdef","abcdf","bcdef","abcdf"])
-- 5353
decode :: ([String], [String]) -> Int
decode (patterns, numbers) = sum $ zipWith (*) digits [1000, 100, 10, 1]
    where
        withTurnedOn    n = fromJust $ find ((== n) . length) patterns
        allWithTurnedOn n = filter ((== n) . length) patterns

        withInCommon n p ps =
            (ps !!) . fromJust . findIndex ((== n) . length) . map (intersect p) $ ps

        notOneOf l ps = head $ ps \\ l

        -- Digits 1,4,7,8 are the ones with, respectively, 2,4,3,7 turned on segments
        [p1,p4,p7,p8] = map withTurnedOn [2,4,3,7]

        -- Digits 2,3,5 have 5 turned on segments, digits 0,6,9 have 6
        patterns235 = allWithTurnedOn 5
        patterns069 = allWithTurnedOn 6

        -- Digit 2 has 2 common segments with 4 and 3 also has 2 with 1.
        -- Digit 5 is the remaining digit with 5 turned on segments.
        p2 = withInCommon 2 p4 patterns235
        p3 = withInCommon 2 p1 patterns235
        p5 = notOneOf [p2,p3] patterns235

        -- Digit 6 has 1 common segments with 1 and 9 has 4 common segments with 4.
        -- Digit 0 is the remaining digit with 6 turned on segments.
        p6 = withInCommon 1 p1 patterns069
        p9 = withInCommon 4 p4 patterns069
        p0 = notOneOf [p6,p9] patterns069

        decoded = [p0,p1,p2,p3,p4,p5,p6,p7,p8,p9]
        digits  = map (fromJust . (`elemIndex` decoded)) numbers
