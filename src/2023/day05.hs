{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec (asciiChar, decimal, endBy, manyTill, sepBy, readParsed, Parser)

import Control.Lens (Field1(_1), Field2(_2), Field3(_3), (^.), over, both)
import Data.Function (on)
import Data.List (sortBy)

type MapRange = (Int, Int, Int) -- dest start, src start, length
type Range    = (Int, Int)      -- start, end

format1 :: Parser ([Int], [[MapRange]])
format1 = (,) <$> ("seeds: " *> seeds) <* "\n\n"
              <*> parseMap `sepBy` "\n"
    where
        seeds = decimal `sepBy` " "

format2 :: Parser ([Range], [[MapRange]])
format2 = (,) <$> ("seeds: " *> seeds) <* "\n\n"
              <*> parseMap `sepBy` "\n"
    where
        seeds    = seedPair `sepBy` " "
        seedPair = (\s l -> (s, s + l - 1))
                       <$> decimal <* " "
                       <*> decimal

parseMap :: Parser [MapRange]
parseMap = manyTill asciiChar "\n" *> range `endBy` "\n"
    where
        range = (,,) <$> decimal <* " " <*> decimal <* " " <*> decimal

findLocation :: [[MapRange]] -> Int -> Int
findLocation maps seed = foldl findNext seed maps

findNext :: Int -> [MapRange] -> Int
findNext cur [] = cur
findNext cur (range : ranges)
    | diff < 0         = cur
    | diff < range^._3 = range^._1 + diff
    | otherwise        = findNext cur ranges
    where
        diff = cur - range^._2

findLocationRange :: [[MapRange]] -> Range -> [Range]
findLocationRange maps seedRange = foldl findNextRanges [seedRange] maps
    where
        findNextRanges :: [Range] -> [MapRange] -> [Range]
        findNextRanges seedRanges ranges = concatMap (findNextRange ranges) seedRanges

findNextRange :: [MapRange] -> Range -> [Range]
findNextRange []               cur = [cur]
findNextRange (range : ranges) cur = 
    case intersectRange cur range of
        (Just left,     Nothing,    Nothing) -> left :            []
        (  Nothing, Just center,    Nothing) ->        t center : []
        (Just left, Just center,    Nothing) -> left : t center : []
        (Just left, Just center, Just right) -> left : t center : findNextRange ranges right
        (  Nothing, Just center, Just right) ->        t center : findNextRange ranges right
        (  Nothing,     Nothing, Just right) ->                   findNextRange ranges right
        where
            t = over both (+ (range^._1 - range^._2))
        

-- intersectRange returns a tuple of
--   1) values to keep as is
--   2) values to modify based on current range
--   3) values to continue trying against next ranges
--  where any of these can be Nothing or Just Range
intersectRange :: Range -> MapRange -> (Maybe Range, Maybe Range, Maybe Range)
intersectRange (s1,e1) (_, s2, l)
    |            e1 <  s2 = (Just (s1,  e1),      Nothing,         Nothing)
    | s1 < s2 && e1 <= e2 = (Just (s1,s2-1), Just (s2,e1),         Nothing)
    | s1 < s2             = (Just (s1,s2-1), Just (s2,e2), Just (e2+1, e1))
    |            e1 <= e2 = (       Nothing, Just (s1,e1),         Nothing)
    |            s1 <= e2 = (       Nothing, Just (s1,e2), Just (e2+1, e1))
    | otherwise           = (       Nothing,      Nothing, Just (s1,   e1))
    where
        e2 = s2 + l - 1

-- |
-- >>> :main
-- 3374647
-- 6082852
main :: IO ()
main = do
    input1 <- readParsed 2023 5 format1

    let seeds1 = fst input1
        maps   = map (sortBy (compare `on` (^._2))) (snd input1)

        minLocation = minimum $ map (findLocation maps) seeds1

    print minLocation


    input2 <- readParsed 2023 5 format2

    let seeds2 = sortBy (compare `on` fst) (fst input2)

        minLocation2 = minimum . map fst $ concatMap (findLocationRange maps) seeds2

    print minLocation2
