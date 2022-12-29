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

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Debug.Trace

type Tunnels = Map String Int
type Valve   = (Int, Tunnels)
type Valves  = Map String Valve

format :: Parser (String, Valve)
format = (,) <$> ("Valve " *> many letterChar)
             <*> valve
    where
        valve   = (,) <$> (" has flow rate=" *> decimal)
                      <*> (prefix *> tunnels)
        prefix  = "; tunnel"   <* optional "s"
                <* " lead"     <* optional "s"
                <* " to valve" <* optional "s"
                <* " "
        tunnels = Map.fromList . map (,1) <$> many letterChar `sepBy1` ", "

squash :: Valves -> ([String], Valves)
squash valves = (starts, squashed')
    where
        starts = filter (`Map.member` squashed') . Map.keys . snd . fromJust $ Map.lookup "AA" valves

        -- remove all 0 rate nodes from the map, adjusting the necessary edges
        noSquash = null . filter ((== 0) . fst . snd) . Map.toList
        squashed = until noSquash squash1 valves

        -- remove 1 move loops
        squashed' = Map.mapWithKey (\k v -> second (Map.delete k) v) squashed

squash1 :: Valves -> Valves
squash1 valves = Map.delete id $ Map.map (second adjust) valves
    where
        (id, (_, edges)) = head . filter ((== 0) . fst . snd) $ Map.toList valves
        adjustedEdges    = Map.map (+1) edges
        
        adjust m = if Map.member id m then Map.delete id $ Map.unionWith min adjustedEdges m else m

maximisePressure :: Valves -> Set String -> Int -> String -> Int
maximisePressure valves open time valve
    | time <= 1 = trace ("t=" ++ show time ++ "\tvalve=" ++ show valve ++ " END") $ 0
    | otherwise = max 
        (trace ("t=" ++ show time ++ "\tvalve=" ++ show valve ++ ", pressure=" ++ show pressure ++ ", open=" ++ show (Set.toList open)) $ pressure + maximum0 ifOpen)
        (trace ("t=" ++ show time ++ "\tvalve=" ++ show valve ++ ", open=" ++ show (Set.toList open)) $ maximum0 ifNotOpen)
    where
        (rate, neighbours) = fromJust $ Map.lookup valve valves
        pressure = (time - 1) * rate
        
        -- filtering here might not yield the right answer
        --neighbours' = filter (`Set.notMember` open) neighbours

        ifOpen    = map (\(v,t) -> maximisePressure valves (Set.insert valve open) (time - t - 1) v) $ Map.toList neighbours
        ifNotOpen = map (\(v,t) -> maximisePressure valves                   open  (time - t)     v) $ Map.toList neighbours

        maximum0    = foldl max 0

-- |
-- >>> :main
-- 
main :: IO ()
main = do
    input <- readParsedLines 2022 16 format

    let time = 5
        
        (starts, valves) = squash (Map.fromList input)
        maxPressure      = maximum $ map (\s -> maximisePressure valves (Set.singleton s) (time - 1) s) starts


    mapM_ print $ Map.toList valves
    --print maxPressure


--memo :: Valves -> Set String -> Int -> String -> Int
--memo valves open time valve =
--    where
--        allValves = map fst $ Map.toList valves
--
--        bob = [ | t <- [0..time],
--                  v <- allValves,
--                  o <- subsets allValves]
--
--        subsets :: Set a -> [Set a]
--        subsets = map Set.fromList f
--            where
--                f [] = [[]]
--                f (x:xs) = f xs ++ map (x:) (f xs)
--
--