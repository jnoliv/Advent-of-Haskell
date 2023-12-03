{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid (Coord, neighbours8)
import Advent.Utils (readAsMap)

import Data.Char (isDigit)
import Data.Function (on)
import Data.List (nub, nubBy)
import Data.Maybe (mapMaybe)

import Data.Map (Map)
import Data.Map qualified as Map

type Number = (Int, Int, Int, Int) -- (y, x, l, n)

positions :: Number -> [Coord]
positions (y,x,l,n) = [(y, x + x') | x' <- [0 .. l-1]]

uid :: Int -> Number -> Int
uid w (y,x,_,_) = y * w + x

parseNumbers :: Int -> String -> [Number]
parseNumbers y = scan 0
    where
        scan x (d : t)
            | isDigit d = scanNumber (x + 1) [d] t
            | otherwise = scan (x + 1) t
        scan x [] = []

        scanNumber x ds (d : t)
            | isDigit d = scanNumber (x + 1) (ds ++ [d]) t
            | otherwise = (y, x - length ds, length ds, read ds) : scan (x + 1) t
        scanNumber x ds [] = [(y, x - length ds, length ds, read ds)]

numbersMap :: (Number -> Int) -> [Number] -> Map Coord (Int, Number)
numbersMap uid numbers = Map.fromList numbersL
    where
        withId      = map (\n -> (uid n, n)) numbers
        withPos num = zip (positions (snd num)) (repeat num)
        numbersL    = concatMap withPos withId

symbolAdjacent :: Map Coord Char -> Number -> Bool
symbolAdjacent symbols num = any (`Map.member` symbols) neighbours
    where
        neighbours = nub . concatMap neighbours8 $ positions num

gearNumbers :: Map Coord (Int, Number) -> Map Coord Char -> [(Int, Int)]
gearNumbers numbers symbols = map (valuePair . snd) gearsWithNumbers
    where
        maybeGears = map fst . Map.toList $ Map.filter (== '*') symbols
        gearsWithNumbers = filter ((== 2) . length . snd) . map (\s -> (s, neighbours s)) $ maybeGears

        neighbours = map snd . nubBy ((==) `on` fst) . mapMaybe (`Map.lookup` numbers) . neighbours8

        valuePair [(_,_,_,n1),(_,_,_,n2)] = (n1,n2)

-- |
-- >>> :main
-- 527446
-- 73201705
main :: IO ()
main = do
    input <- readInputDefaults 2023 3

    let width = length . head $ lines input

        symbols = readAsMap (\c -> if c == '.' || isDigit c then Nothing else Just c) input

        numbers = concatMap (uncurry parseNumbers) . zip [0..] $ lines input
        asMap = numbersMap (uid width) numbers

        adjacent = filter (symbolAdjacent symbols) numbers
        
    print . sum $ map (\(_, _, _, n) -> n) adjacent

    print . sum . map (uncurry (*)) $ gearNumbers asMap symbols
