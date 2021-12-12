{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid (Coord, neighbours8)
import Advent.Utils (readAsMap, showMap)
import Data.Char (digitToInt, intToDigit)
import Data.List (findIndex)
import Data.Maybe (fromJust)
import Data.Map (Map)
import Data.Map qualified as Map

type Cave = Map Coord Int

-- |
-- >>> :main
-- 1634
-- 210
main :: IO ()
main = do
    input <- readInputDefaults 2021 11

    let octopi = readAsMap (Just . digitToInt) input
        steps  = iterate step (octopi, 0)
        count  = Map.size octopi

    print . sum . map snd $ take 101 steps
    print . fromJust $ findIndex ((== count) . snd) steps

step :: (Cave, Int) -> (Cave, Int)
step (cave, _) = step' cave' 0
    where
        cave' = Map.map (+1) cave

        step' m c
            | null willFlash = (Map.map (\i -> if i < 0 then 0 else i) m, c)
            | otherwise      = step' m' (c + length willFlash)
            where
                willFlash = map fst . Map.toList $ Map.filter (> 9) m
                m'        = foldl flash m willFlash

flash :: Cave -> Coord -> Cave
flash m k = foldl (\m' k' -> Map.insertWith (+) k' 1 m') (Map.insert k (-10) m) ns
    where
        ns = filter (`Map.member` m) $ neighbours8 k
        