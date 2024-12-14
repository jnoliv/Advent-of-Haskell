{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, TupleSections #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec (Parser, readParsedLines, sdecimal)
import Advent.Coord.Grid ((.*), (.+), Coord, neighbours4)
import Advent.Utils (count)
import Data.Bifunctor (bimap)
import Data.List (intercalate)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

parser :: Parser (Coord, Coord)
parser = (,) <$> pos <* " " <*> vel
    where
        pos = (,) <$> ("p=" *> sdecimal) <* "," <*> sdecimal
        vel = (,) <$> ("v=" *> sdecimal) <* "," <*> sdecimal

move :: (Int, Int) -> Int -> (Coord,Coord) -> (Coord)
move (lx,ly) n (pos,vel) = wrap $ pos .+ (n .* vel)
    where
        wrap (x,y) = bimap posX posY (x `mod` lx, y `mod` ly)

        posX x = if x >= 0 then x else x + lx
        posY y = if y >= 0 then y else y + ly

safety :: (Int,Int) -> [Coord] -> Int
safety (lx,ly) = product . sequence [countQuad (<) (<), countQuad (>) (<), countQuad (<) (>), countQuad (>) (>)]
    where
        countQuad fx fy = length . filter (inQuad fx fy)
        inQuad    fx fy (x,y) = x `fx` (lx `div` 2) && y `fy` (ly `div` 2)

countSurrounded :: [Coord] -> Int
countSurrounded poss = count surrounded poss
    where
        asSet = Set.fromList poss

        surrounded = (== 4) . count (`Set.member` asSet) . neighbours4

showRobots :: (Int,Int) -> [Coord] -> String
showRobots (lx,ly) poss = intercalate "\n" [[showCount x y | x <- [0 .. lx - 1]] | y <- [0 .. ly - 1]] ++ "\n"
    where
        counts = Map.fromListWith (+) $ map (, 1) poss

        showCount x y = (\n -> if n == 0 then ' ' else '█') $ Map.findWithDefault 0 (x,y) counts

-- |
-- >>> :main
-- 228690000
-- 7093
main :: IO ()
main = do
    input <- readParsedLines 2024 14 parser

    let dims = (101, 103)

    print . safety dims . map (move dims 100) $ input

    -- After trying several heuristics unsuccessfully, I thought that checking
    -- for bunched up robots would work. Over 20 together was my first guess.
    let (secs, robots) = head . dropWhile ((< 20) . countSurrounded . snd) $
            [(n,) $ map (move dims n) input | n <- [1..]]
    
    print secs

    -- Uncomment to see the image.
    -- putStrLn . showRobots dims $ robots
