{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec (Parser, readParsedLines, sepBy, decimal)
import Advent.Coord.Grid (Coord, (.-), (.+), down, left, right)
import Advent.Utils (findDefininingPoints)
import Data.Bifunctor (bimap)
import Data.List (nub)

import Data.Map (Map)
import Data.Map qualified as Map

data Object = Rock | Sand | Air
    deriving Eq

type Cave = Map Coord Object

format :: Parser [Coord]
format = coord `sepBy` " -> "
    where coord = flip (,) <$> decimal <* "," <*> decimal

expand :: [Coord] -> [Coord]
expand cs = nub . concatMap line $ pairs
    where
        getDir (c1, c2) = bimap signum signum (c2 .- c1)
        line p@(c1, c2) = (++ [c2]) . takeWhile (/= c2) $ iterate (.+ (getDir p)) c1

        pairs = zip cs (tail cs)

-- Part 1

whileUnique :: (Cave -> Cave) -> Cave -> Cave
whileUnique f cave = fst . head . dropWhile (uncurry (/=)) $ zip caves (tail caves)
    where 
        caves = iterate f cave

produceSand :: Int -> Coord -> Cave -> Cave
produceSand ground source cave =
    if null atRest
        then cave
        else (\(s,c) -> Map.insert s Sand c) . head $ atRest
    where
        fallingToVoid (y, _) = y > ground

        dropWhileMoving (h1@(s1, _) : h2@(s2, _) : t)
            | fallingToVoid s1 = []
            | s1 /= s2         = dropWhileMoving (h2 : t)
            | otherwise        = h1 : h2 : t
        
        atRest = dropWhileMoving $ iterate dropSand (source, cave)

dropSand :: (Coord, Cave) -> (Coord, Cave)
dropSand (sand, cave) =
    if null validMoves
        then (sand, cave)
        else (head validMoves, cave)
    where
        moves      = map ($ sand) [down, down . left, down . right]
        validMoves = filter (`Map.notMember` cave) moves

-- Part 2

produceSandWithGround :: Int -> Coord -> Cave -> Cave
produceSandWithGround ground source cave =
    if null atRest
        then cave
        else (\(s,c) -> Map.insert s Sand c) . head $ atRest
    where
        blockingSource = (==) source

        dropWhileMoving (h1@(s1, _) : h2@(s2, _) : t)
            | blockingSource s2 = []
            | s1 /= s2          = dropWhileMoving (h2 : t)
            | otherwise         = h1 : h2 : t
        
        atRest = dropWhileMoving $ iterate (dropSandWithGround ground) (source, cave)

dropSandWithGround :: Int -> (Coord, Cave) -> (Coord, Cave)
dropSandWithGround ground (sand, cave) =
    if null validMoves
        then (sand, cave)
        else (head validMoves, cave)
    where
        moves      = map ($ sand) [down, down . left, down . right]
        validMoves = filter (\(y,x) -> y < ground && Map.notMember (y,x) cave) moves

-- |
-- >>> :main
-- 1406
-- 20870
main :: IO ()
main = do
    let source = (0,500) --(y,x)

    input <- readParsedLines 2022 14 format

    let points  = concatMap expand input
        caveMap = Map.fromList $ zip points (repeat Rock)

        (_, (ground, _)) = findDefininingPoints . map fst $ Map.toList caveMap

    let withSand  = whileUnique (produceSand ground source) caveMap
        countSand = Map.size $ Map.filter (== Sand) withSand

    print countSand

    let withSand'  = whileUnique (produceSandWithGround (ground + 2) source) caveMap
        countSand' = Map.size $ Map.filter (== Sand) withSand'

    print $ countSand' + 1
