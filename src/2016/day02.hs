{-# LANGUAGE ImportQualifiedPost #-}

import AdventAPI (readInputDefaults)
import Advent.Coord.Grid
import Advent.Utils (readAsMap)
import Data.Maybe (fromJust)
import Data.Map (Map)
import Data.Map qualified as Map

type Keypad = Map.Map (Int, Int) Char

move :: Keypad -> Coord -> Char -> Coord
move keypad coord dir
    | coord' `Map.member` keypad = coord'
    | otherwise                  = coord
    where
        coord' = f dir $ coord
        f 'U'  = up
        f 'R'  = right
        f 'D'  = down
        f 'L'  = left

code :: Keypad -> Coord -> [String] -> String
code _      _   []       = []
code keypad pos (i : is) = button : code keypad pos' is
    where
        pos'   = foldl (move keypad) pos i
        button = fromJust $ Map.lookup pos' keypad

-- |
-- >>> :main
-- 82958
-- B3DB8
main :: IO ()
main = do
    instrs <- lines <$> readInputDefaults 2016 2

    let parseButton c
            | c /= '#'  = Just c
            | otherwise = Nothing

        keypad1 = readAsMap parseButton "123\n456\n789\n"
        keypad2 = readAsMap parseButton "##1\n#234\n56789\n#ABC\n##D"

    putStrLn $ code keypad1 (1,1) instrs
    putStrLn $ code keypad2 (1,1) instrs
