{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, TupleSections #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec (readParsed, upperChar, endBy, some, Parser)
import Data.Bifunctor (second)
import Data.Maybe (fromJust)
import Data.Map (Map)
import Data.Map qualified as Map

type Node = (String,String)

parser :: Parser (String, [(String,Node)])
parser = (,) <$> some upperChar <* "\n\n"
             <*> parseAdj `endBy` "\n"
    where
        parseAdj  = (,) <$> some upperChar <* " = ("
                        <*> parseNext      <* ")"
        parseNext = (,) <$> some upperChar <* ", "
                        <*> some upperChar

navigate :: Map String Node -> (String, [Node -> String]) -> (String, [Node -> String])
navigate adj (cur, f : instrs) = (,instrs) . f . fromJust $ Map.lookup cur adj

stepsWhile :: Map String Node -> [Node -> String] -> (String -> Bool) -> String -> Int
stepsWhile adj transfs pred start = length . takeWhile (pred . fst) $ iterate (navigate adj) (start, cycle transfs)

-- |
-- >>> :main
-- 22199
-- 13334102464297
main :: IO ()
main = do
    (instrs, adjList) <- readParsed 2023 8 parser

    let adj     = Map.fromList adjList
        transfs = map (\c -> if c == 'L' then fst else snd) instrs

        starts = filter ((== 'A') . last) $ Map.keys adj

        steps = stepsWhile adj transfs (/= "ZZZ") "AAA"

        stepsGhosts = map (stepsWhile adj transfs ((/= 'Z') . last)) starts
        stepsGhost  = foldr1 lcm stepsGhosts

    print steps
    print stepsGhost
