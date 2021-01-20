{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec
import Data.List (find)

type Prop = (String, Int)

format :: Parser (Int, [Prop])
format = (,) <$> ("Sue " *> decimal <* ": ")
             <*> (property `sepBy` ", ")
    where
        property = (,) <$> many letterChar <* ": " <*> decimal

filterAunts :: ([Prop] -> Prop -> Bool) -> [Prop] -> [(Int, [Prop])] -> Int
filterAunts f props = fst . head . filter (all (f props) . snd)

part1 :: [Prop] -> Prop -> Bool
part1 props (k, v) = case find ((== k) . fst) props of
    Just (_, n) -> n == v
    Nothing     -> True

part2 :: [Prop] -> Prop -> Bool
part2 props (k, v) = case find ((== k) . fst) props of
    Just ("cats", n)        -> v > n
    Just ("trees", n)       -> v > n
    Just ("pomeranians", n) -> v < n
    Just ("goldfish", n)    -> v < n
    Just (_, n)             -> n == v
    Nothing                 -> True

-- |
-- >>> :main
-- 40
-- 241
main :: IO ()
main = do
    aunts <- readParsedLines 2015 16 format

    let tickertape = [("children", 3), ("cats", 7), ("samoyeds", 2), ("pomeranians", 3), ("akitas", 0),
                      ("vizslas", 0), ("goldfish", 5), ("trees", 3), ("cars", 2), ("perfumes", 1)]

    print $ filterAunts part1 tickertape aunts
    print $ filterAunts part2 tickertape aunts
