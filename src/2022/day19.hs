{-# LANGUAGE ImportQualifiedPost, OverloadedStrings, BlockArguments, TupleSections #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec
import Advent.Coord.Grid
import Advent.Life
import Advent.Math
import Advent.Utils

import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe

--import Data.List.Split

--import Data.Map (Map)
--import Data.Map qualified as Map
--import Data.Set (Set)
--import Data.Set qualified as Set

import Debug.Trace

data Resource  = Ore | Clay | Obsidian | Geode      deriving Show
data Robot     = Robot Resource [(Resource, Int)]   deriving Show
type Blueprint = [Robot]

type Resources = (Int, Int, Int, Int)
type Robots    = (Int, Int, Int, Int)
type State     = (Robots, Resources)

format :: Parser Blueprint
format = trim "Blueprint " decimal ":" *> many robot
    where
        robot           = Robot <$> trim " Each " resource " robot costs "
                                <*> resourceAmounts <* "."

        resourceAmounts = resourceAmount `sepBy1` " and "
        resourceAmount  = flip (,) <$> decimal <* " " <*> resource
        resource        = try (Ore      <$ "ore")
                      <|>     (Clay     <$ "clay")
                      <|>     (Obsidian <$ "obsidian")
                      <|>     (Geode    <$ "geode")

maximiseGeodes :: Blueprint -> Int -> Int
maximiseGeodes blueprint time = getNumGeodes . traceShowId $ maximise time ((1,0,0,0), (0,0,0,0))
    where
        getNumGeodes (_,(_,_,_,g)) = g

        maxGeodes r1@(_,(_,_,_,g1)) r2@(_,(_,_,_,g2)) = if g1 > g2 then r1 else r2

        maximise :: Int -> State -> State
        maximise 0 state = state
        maximise t state = foldl maxGeodes (collectN t state) branches
            where
                waitAndCraft (r, d) = maximise (t - d) $ craft (collectN d state) r

                withTimes = mapMaybe (\r -> (r,) <$> timeToCraft state r) blueprint
                stillTime = filter ((< t) . snd) withTimes
                branches  = map waitAndCraft stillTime

collectN :: Int -> State -> State
collectN n (r@(rOre, rClay, rObsidian, rGeode), (ore, clay, obsidian, geode)) =
    (r, (ore + n * rOre, clay + n * rClay, obsidian + n * rObsidian, geode + n * rGeode))

timeToCraft :: State -> Robot -> Maybe Int
timeToCraft ((rOre, rClay, rObsidian, rGeode), (ore, clay, obsidian, geode)) (Robot _ resources) =
    maximum $ map waitTime resources
    where
        waitTime (Ore,      n) = if rOre      == 0 then Nothing else Just $ n - ore      `div` rOre
        waitTime (Clay,     n) = if rClay     == 0 then Nothing else Just $ n - clay     `div` rClay
        waitTime (Obsidian, n) = if rObsidian == 0 then Nothing else Just $ n - obsidian `div` rObsidian

craft :: State -> Robot -> State
craft ((rOre, rClay, rObsidian, rGeode), (ore, clay, obsidian, geode)) robot =
    case robot of
        (Robot Ore      [(Ore,n)])               -> ((rOre + 1, rClay, rObsidian, rGeode), (ore - n, clay,     obsidian, geode))
        (Robot Clay     [(Ore,n)])               -> ((rOre, rClay + 1, rObsidian, rGeode), (ore - n, clay,     obsidian, geode))
        (Robot Obsidian [(Ore,n), (Clay,m)])     -> ((rOre, rClay, rObsidian + 1, rGeode), (ore - n, clay - m, obsidian, geode))
        (Robot Geode    [(Ore,n), (Obsidian,m)]) -> ((rOre, rClay, rObsidian, rGeode + 1), (ore - n, clay,     obsidian, geode - m))

-- |
-- >>> :main
-- 
main :: IO ()
main = do
    input <- readParsedLines 2022 19 format

    mapM_ print $ head input

    let numGeodes = maximiseGeodes (head input) 12

    print numGeodes

    --let state  = ((1,0,0,0), (0,0,0,0))
    --    state4 = collectN 4 state
    --
    --    stateG = ((1,0,0,0), (0,0,0,0))
    --
    --print $ timeToCraft state  (Robot Ore [(Ore,4)])
    --print $ timeToCraft state4 (Robot Ore [(Ore,4)])
    -- 
    --mapM_ print input
    --print state4
    --print $ canCraftWith state4 (Robot Ore [(Ore,4)])
    --print $ craft        state4 (Robot Ore [(Ore,4)])

    --let t = 24
    --    
    --    withTimes = map (\r -> (r, timeToCraft state r)) (head input)
    --    stillTime = filter ((< t) . snd) withTimes
    --
    --    branches  = map (\(r,d) -> (t-d, collectN d state)) stillTime
    --
    --mapM_ print withTimes
    --putStr "\n"
    --mapM_ print stillTime
    --putStr "\n"
    --mapM_ print branches
