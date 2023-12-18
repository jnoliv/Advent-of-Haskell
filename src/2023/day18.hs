{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec (hexDigitChar, letterChar, decimal, some, readParsedLines, Parser)
import Advent.Coord.Grid ((.*), (.+), up, left, down, origin, right, Coord)
import Advent.Utils (sinsert)

import Data.Bifunctor (first, bimap)
import Data.Function (on)
import Data.List (find, delete, groupBy, sortOn)
import Data.Maybe (fromJust, isJust)

import Numeric (readHex)
import Data.Tuple (swap)

type RawInstr = (Char,Int)
type    Instr = (Coord, Int)

parser :: Parser (RawInstr,RawInstr)
parser = (,) <$> instr1 <* " (#" <*> instr2 <* ")"
    where
        instr1 = (,) <$> letterChar <* " " <*> decimal
        instr2 = bimap head toHex . swap . splitAt 5 <$> some hexDigitChar

        toHex  = fst . head. readHex

toDir :: Char -> Coord
toDir c
    | c `elem` ("R0" :: String) = right origin
    | c `elem` ("D1" :: String) = down  origin
    | c `elem` ("L2" :: String) = left  origin
    | c `elem` ("U3" :: String) = up    origin

walk :: [Instr] -> [(Coord, Bool)]
walk instructions = step origin [] instructions
    where
        step cur vs ((dir, n) : instrs)
            | null instrs = vs'
            | otherwise   = step next vs' instrs
            where
                next = cur .+ (n .* dir)
                vs'  = (next, isBegin) : vs

                isBegin = dir' == right origin || dir == left origin

                -- On the last iteration, use the first direction in the original instructions as "next" direction.
                dir' = fst . head $ (if null instrs then instructions else instrs)

area :: [Instr] -> Int
area instructions = sweep 0 [] 0 nubbed
    where
        -- All vertices paired with whether they begin or end a square.
        vertices = walk instructions
        -- Group all vertices by column.
        grouped  = groupBy ((==) `on` (snd . fst)) $ sortOn (swap . fst) vertices
        nubbed   = map (\vs -> ((snd . fst) (head vs), map simplify vs)) grouped

        simplify ((r,c),b) = (r,b)

        sweep :: Int -> [(Int,Int)] -> Int -> [(Int,[(Int,Bool)])] -> Int
        sweep accum   []       _                     [] = accum
        sweep accum open colPrev ((col, rows) : events) = sweep accum' open' col events
            where
                (open', se) = openClose 0 open rows
                accum'      = accum + se + sum (map (\pair -> (col - colPrev) * getHeight pair) open)

                getHeight pair = snd pair - fst pair + 1

                openClose :: Int -> [(Int,Int)] -> [(Int,Bool)] -> ([(Int,Int)], Int)
                openClose specialEdges open                                 [] = (open, specialEdges)
                openClose specialEdges open ((r0,isBeg0) : (r1,isBeg1) : rows)
                    |     isBeg0 &&     isBeg1 = openClose (specialEdges +  seSplit)                split rows
                    | not isBeg0 && not isBeg1 = openClose (specialEdges + seMerged)               merged rows
                    | not isBeg0 &&     isBeg1 = openClose (specialEdges +      se0) (sinsert new0 open0) rows
                    |     isBeg0 && not isBeg1 = openClose (specialEdges +      se1) (sinsert new1 open1) rows
                    where
                        maybeMatch0 = find (match r0) open
                        maybeMatch1 = find (match r1) open

                        maybeWithin = find ((r0,r1) `isWithin`) open
                        within = fromJust maybeWithin

                        split  = if isJust maybeWithin
                                    then sinsert (fst within, r0) . sinsert (r1, snd within) $ delete within open
                                    else sinsert (r0,r1) open
                        seSplit = if isJust maybeWithin then r1 - r0 - 1 else 0

                        merged = if maybeMatch0 == maybeMatch1
                                    then delete (r0,r1) open
                                    else sinsert (fst match0, snd match1) . delete match1 $ delete match0 open
                        seMerged = if maybeMatch0 == maybeMatch1 then r1 - r0 + 1 else 0

                        match0 = fromJust maybeMatch0
                        open0  = delete match0 open
                        new0   = if fst match0 == r0 then (r1, snd match0) else (fst match0, r1)
                        se0    = if fst match0 == r0 then r1 - r0          else 0

                        match1 = fromJust maybeMatch1
                        open1  = delete match1 open
                        new1   = if fst match1 == r1 then (r0, snd match1) else (fst match1, r0)
                        se1    = if fst match1 == r1 then 0                else r1 - r0

                isWithin (r0,r1) (r0',r1') = r0' <= r0 && r1 <= r1'
                match r  (r0,r1)           = r == r0 || r == r1

-- |
-- >>> :main
-- 36679
-- 88007104020978
main :: IO ()
main = do
    input <- readParsedLines 2023 18 parser

    let instructions1 = map (first toDir . fst) input
        instructions2 = map (first toDir . snd) input

    print $ area instructions1
    print $ area instructions2
