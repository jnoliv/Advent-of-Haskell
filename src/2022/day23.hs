{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid
import Advent.Utils (readAsSet, findDefininingPoints)
import Data.Maybe (mapMaybe)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

type Rule = (Set Coord -> Coord -> Maybe Coord)

rules :: [Rule]
rules = [north, south, west, east]
    where
        f s c cs = if all (`Set.notMember` s) cs
            then Just c
            else Nothing

        north s c = f s (up   c) $ map ($ c) [up   . left, up,   up   . right]
        south s c = f s (down c) $ map ($ c) [down . left, down, down . right]
        
        west s c = f s (left  c) $ map ($ c) [left  . up, left,  left  . down]
        east s c = f s (right c) $ map ($ c) [right . up, right, right . down]

simulate :: (Set Coord, [Rule], Bool) -> (Set Coord, [Rule], Bool)
simulate (elves, rules, _) = (elves', rules', Map.size count > 0)
    where
        (proposed, count) = Set.foldl (propose elves rules) (Map.empty, Map.empty) elves
        elves'            = Map.foldlWithKey (move count) Set.empty proposed

        rules'            = tail rules ++ [head rules]

propose :: Set Coord -> [Rule] -> (Map Coord Coord, Map Coord Int) -> Coord -> (Map Coord Coord, Map Coord Int)
propose elves rules (proposed, count) elf
    | noMove    = (Map.insert elf elf         proposed, count)
    | otherwise = (Map.insert elf proposition proposed, Map.insertWith (+) proposition 1 count)
    where
        neighbours   = filter (`Set.member` elves) $ neighbours8 elf
        propositions = mapMaybe (\r -> r elves elf) rules
        proposition  = head propositions

        noMove = null neighbours || null propositions

move :: Map Coord Int -> Set Coord -> Coord -> Coord -> Set Coord
move count elves cur new =
    case new `Map.lookup` count of
        Just 1  -> Set.insert new elves
        Just _  -> Set.insert cur elves
        Nothing -> Set.insert cur elves

countEmpty :: Set Coord -> Int
countEmpty elves = (y1 - y0 + 1) * (x1 - x0 + 1) - Set.size elves
    where
        ((y0,x0), (y1,x1)) = findDefininingPoints $ Set.toList elves

-- |
-- >>> :main
-- 4091
-- 1036
main :: IO ()
main = do
    elves <- readAsSet (== '#') <$> readInputDefaults 2022 23

    let simulated    = iterate simulate (elves, rules, True)
        emptyAfter10 = countEmpty . (\(s,_,_) -> s) $ simulated !! 10
        
        untilAllStop = takeWhile (\(_,_,b) -> b) simulated

    print emptyAfter10 
    print $ length untilAllStop
