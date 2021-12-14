{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec (readParsed, letterChar, endBy1, some, Parser)
import Data.Array((!), listArray)
import Data.Function (on)
import Data.List (elemIndex, sortBy)
import Data.Maybe (fromJust)
import Data.Map (Map)
import Data.Map qualified as Map

format :: Parser (String, Map String Char)
format = (,) <$> template <* "\n\n" <*> rules
    where
        template = some letterChar
        rules    = Map.fromList <$> rule `endBy1` "\n"
        rule     = (,) <$> some letterChar <* " -> " <*> letterChar

-- |
-- >>> :main
-- 3095
-- 3152788426516
main :: IO ()
main = do
    (template, rules) <- readParsed 2021 14 format

    print $ answer rules 10 template
    print $ answer rules 40 template

-- Returns the subtraction between the most and least common elements in the
-- polymer resulting from applying the pair insertion steps depth times.
answer :: Map String Char -> Int -> String -> Int
answer rules depth polymer = snd (last sorted) - snd (head sorted)
    where
        counts = dac rules depth polymer
        sorted = sortBy (compare `on` snd) $ Map.toList counts

-- This is some kind of memoized divide and conquer, hence the function name.
-- Since each starting pair will only create elements in the middle, the total
-- count is the sum of counts of whatever each pair creates plus the count
-- of elements in the initial polymer.
dac :: Map String Char -> Int -> String -> Map Char Int
dac rules depth polymer =
    -- Sum all counts from all pairs at given depth.
    foldl (Map.unionWith (+)) initCounts $ map (\k -> memo ! index k depth) pairs
    where
        -- All pairs of initial polymer.
        pairs      = zipWith (\a b -> [a,b]) polymer (tail polymer)
        -- Element count in initial polymer.
        initCounts = foldl (\m c -> Map.insertWith (+) c 1 m) Map.empty polymer

        keys = map fst $ Map.toList rules
        n    = length keys

        -- Computes the index in the memoized array from the pair and depth,
        -- where depth is actually the number of steps to go.
        index k d = (fromJust $ k `elemIndex` keys, d)

        -- The actual memoized array. Lazy evaluation means we compute only what we need.
        memo = listArray ((0,0), (n - 1, depth)) [f k d | k <- keys, d <- [0 .. depth]]

        -- The actual logic. A value in the table is the sum of counts of the new
        -- pairs plus the added character.
        f [a,b] 0 = Map.empty
        f [a,b] d = count c (memo ! index [a,c] (d-1)) (memo ! index [c,b] (d-1))
            where
                c             = fromJust $ Map.lookup [a,b] rules
                count k m1 m2 = Map.insertWith (+) k 1 $ Map.unionWith (+) m1 m2
