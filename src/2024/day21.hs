{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, BlockArguments, LambdaCase #-}

import Advent.Megaparsec (readParsedLines, alphaNumChar, many)
import Advent.Coord.Grid (distance, down, left, right, up, Coord)
import Advent.Utils (combinations, readAsMap)
import Data.Bool (bool)
import Data.List (group)
import Data.MemoTrie (memo3)

import Data.Map (Map, (!))
import Data.Map qualified as Map

type Keypad = Map Coord Char
type Paths  = Map (Char,Char) [String]

numeric, directional :: Keypad
numeric     = readAsMap (bool Nothing <$> Just <*> (/= ' ')) . unlines $ [
        "789",
        "456",
        "123",
        " 0A"
    ]
directional = readAsMap (bool Nothing <$> Just <*> (/= ' ')) . unlines $ [
        " ^A",
        "<v>"
    ]

paths :: Keypad -> Paths
paths keypad = addAs . addSelfs . best . Map.fromList $ map toKV nodes
    where
        addAs       = Map.map (map (++ "A"))
        best        = Map.map (filter ((<= 2) . length . group))
        addSelfs ps = foldl (\m n -> Map.insert (keypad ! n,keypad ! n) [""] m) ps $ Map.keys keypad
        toKV [a,b]  = ((keypad ! a, keypad ! b), getPaths a b)
        nodes       = ((++) <$> id <*> map reverse) . combinations 2 $ Map.keys keypad

        getPaths :: Coord -> Coord -> [String]
        getPaths start end
            | start == end = [[]]
            | otherwise    = concatMap (\(p,c) -> map (c :) $ getPaths p end) nexts
            where
                valid = (&&) <$> (`Map.member` keypad)
                             <*> (distance start end >) . distance end
                nexts = filter (valid . fst) [
                        (up    start, '^'),
                        (right start, '>'),
                        (down  start, 'v'),
                        (left  start, '<')
                    ]

minPresses :: Paths -> Paths -> Int -> String-> Int
minPresses pathsNum pathsDir max code = cost 0 code
    where
        cost d code = sum $ zipWith (go d) ('A' : code) code

        go :: Int -> Char -> Char -> Int
        go = memo3 $ \cases
            0 cur tgt           -> minimum . map (cost 1      ) $ pathsNum ! (cur,tgt)
            d cur tgt | d < max -> minimum . map (cost (d + 1)) $ pathsDir ! (cur,tgt)
            d cur tgt           -> minimum . map length         $ pathsDir ! (cur,tgt)

complexity :: String -> Int -> Int
complexity code length = read (init code) * length

-- |
-- >>> :main
-- 248108
-- 303836969158972
main :: IO ()
main = do
    codes <- readParsedLines 2024 21 (many alphaNumChar)

    let (pathsNum, pathsDir) = (paths numeric, paths directional)

    print . sum . map (complexity <$> id <*> minPresses pathsNum pathsDir  2) $ codes
    print . sum . map (complexity <$> id <*> minPresses pathsNum pathsDir 25) $ codes
