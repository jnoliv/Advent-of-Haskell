{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid (Coord, neighbours8)
import Advent.Utils (readAsMap, showMap, count, replaceString)
import Data.Char (digitToInt, intToDigit)
import Data.List (findIndex)
import Data.Maybe (fromJust)
import Data.Map (Map)
import Data.Map qualified as Map

import Control.Concurrent (threadDelay)
import Visualize
import System.Environment (getArgs)

type Cave = Map Coord Int

-- |
-- >>> :main
-- 1634
-- 210
main :: IO ()
main = do
    args <- getArgs
    let doVis = not (null args) && (head args == "-v")

    input <- readInputDefaults 2021 11

    let octopuses  = readAsMap (Just . digitToInt) input
        steps      = iterate step (octopuses, 0)
        nOctopuses = Map.size octopuses

    if doVis
        then visualize (count (== '\n') input) $ map fst steps
        else do
            print . sum . map snd $ take 101 steps
            print . fromJust $ findIndex ((== nOctopuses) . snd) steps

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

visualize :: Int -> [Cave] -> IO ()
visualize rows caves = do
    putStr $ replicate rows '\n'

    cursorUp rows
    saveCursorPosition

    let visCave cave = do
            restoreCursorPosition
            putStr "\ESC[40m"

            let vis = showMap (\i -> if i == 9 then '#' else ' ') 0 cave

            putStr $ replaceString "#" "\ESC[103m \ESC[40m" vis
            putStr "\n"
            threadDelay 100000

    mapM_ visCave caves
    putStr "\ESC[0m"
