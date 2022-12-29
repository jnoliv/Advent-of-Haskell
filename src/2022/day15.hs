{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec (Parser, readParsedLines, trim, sdecimal)
import Advent.Coord.Grid (Coord, origin, distance)
import Data.List (sort)

import Data.Map (Map)
import Data.Map qualified as Map

data Spot = Sensor | Beacon | Empty | Unknown
    deriving Eq

format :: Parser (Coord, Coord)
format = (,) <$> sensor <*> beacon
    where
        sensor = flip (,) <$> trim "Sensor at x="            sdecimal ", "
                          <*> trim "y="                      sdecimal ": "
        beacon = flip (,) <$> trim "closest beacon is at x=" sdecimal ", "
                          <*>      ("y=" *>                  sdecimal)

mapBeacon :: Int -> Map Coord Spot -> (Coord, Coord) -> Map Coord Spot
mapBeacon row1 tMap (sensor, beacon) = tMap'
    where
        withBeacon = Map.insert beacon Beacon tMap
        withBoth   = Map.insert sensor Sensor withBeacon

        dist   = distance sensor beacon
        points = emptyPoints row1 sensor dist

        tMap'  = foldl (\m k -> Map.insertWith (\n o -> o) k Empty m) withBoth points

emptyPoints :: Int -> Coord -> Int -> [Coord]
emptyPoints row (y,x) dist
    | hitsRow row (y,x) dist = [(row, x') | x' <- [x - range .. x + range]]
    | otherwise              = []
    where
        range = dist - abs (y - row)

hitsRow :: Int -> Coord -> Int -> Bool
hitsRow row (y,x) dist = y - dist <= row && row <= y + dist

-- Part 2
findBeacon :: Int -> [(Coord, Int)] -> Coord -> Coord
findBeacon range sensors cur@(y,x)
    | null detecting = cur
    | newX > range   = findBeacon range sensors (y + 1, 0)
    | otherwise      = findBeacon range sensors (y, newX)
    where
        detecting = filter (\(s,d) -> distance cur s <= d) sensors

        distToRow (y',_) = abs (y' - y)

        newXs = map (\(s@(_,x'), d) -> x' + d - distToRow s + 1) detecting
        newX  = maximum newXs

-- |
-- >>> :main
-- 4861076
-- 10649103160102
main :: IO ()
main = do
    input <- readParsedLines 2022 15 format

    let row1 = 2000000
    
        rowMap = foldl (mapBeacon row1) Map.empty input
        empty  = Map.size $ Map.filter (== Empty) rowMap
    
    print empty

    let range   = 4000000
        beacons = map (\(s,b) -> (s, distance s b)) $ sort input

        p       = findBeacon range beacons origin

    print . (\(y,x) -> y + range * x) $ p
