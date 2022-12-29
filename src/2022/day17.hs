{-# LANGUAGE ImportQualifiedPost, TupleSections #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid
import Advent.Utils (showMap, findDefininingPoints)
import Data.Bifunctor (first)
import Data.Maybe (fromJust)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

data Obj = Ground | Wall | Corner | Rock | FallingRock | None
    deriving Eq

type Rock = [Coord]

rock_hLine = [(-4,3), (-4,4), (-4,5), (-4,6)] -- ####

rock_plus = [        (-6,4),                  -- .#.
             (-5,3), (-5,4), (-5,5),          -- ###
                     (-4,4)]                  -- .#.

rock_invL = [                (-6,5),          -- ..#
                             (-5,5),          -- ..#
             (-4,3), (-4,4), (-4,5)]          -- ###

rock_vLine = [(-7,3),                         -- #
              (-6,3),                         -- #
              (-5,3),                         -- #
              (-4,3)]                         -- #

rock_square = [ (-5,3), (-5,4),               -- ##
                (-4,3), (-4,4)]               -- ##

rocks = [rock_hLine, rock_plus, rock_invL, rock_vLine, rock_square]

simulate :: Int -> (Int,Int) -> [(Rock, Int)] -> [(Char, Int)] -> Map Coord Obj -> Map Coord Obj
simulate 0 _     _            _    board = board
simulate n walls (rock : rocks) jets board =
    simulate (n-1) walls rocks jets' trimmed
    where
        (placedRock, board') = placeRock walls (fst rock) board
        (jets', board'')     = simulateRock jets board' placedRock

        (_, _, trimmed)      = trimTower walls board''

placeRock :: (Int,Int) -> Rock -> Map Coord Obj -> (Rock, Map Coord Obj)
placeRock walls newRock board = (placedRock, withNewWalls)
    where
        ((top,_),(bot,_)) = findDefininingPoints . Map.keys . Map.filter (== Rock) $ board

        placedRock = map (.+ (top,0)) newRock

        newHeight  = minimum $ map fst placedRock

        withNewWalls = Map.union board . Map.fromList $
            [((y, fst walls), Wall)   | y <- [newHeight..bot]] ++
            [((y, snd walls), Wall)   | y <- [newHeight..bot]]

simulateRock :: [(Char, Int)] -> Map Coord Obj -> Rock -> ([(Char, Int)], Map Coord Obj)
simulateRock ((jet,_) : jets) objs rock
    | stop      = (jets, objs')
    | otherwise = simulateRock jets objs rock''
    where
        rock'          = applyJet objs jet rock
        (rock'', stop) = applyGravity objs rock'

        objs'          = foldl (\m k -> Map.insert k Rock m) objs rock'

applyJet :: Map Coord Obj -> Char -> Rock -> Rock
applyJet objs jet rock
    | collides objs rock' = rock
    | otherwise           = rock'
    where
        push '>' = map right
        push '<' = map left

        rock' = push jet rock

applyGravity :: Map Coord Obj -> Rock -> (Rock, Bool)
applyGravity objs rock
    | collides objs rock' = (rock , True)
    | otherwise           = (rock', False)
    where
        rock' = map down rock

collides :: Map Coord Obj -> Rock -> Bool
collides objs = any (`Map.member` objs)

rockHeight :: Map Coord Obj -> Int
rockHeight = negate . fst . fst . findDefininingPoints . Map.keys . Map.filter (== Rock)

findCycle :: (Int, Int) -> [(Rock, Int)] -> [(Char, Int)] -> Map Coord Obj -> ((Int, Int), (Int, Int))
findCycle = findCycle' 0 Map.empty
    where
        findCycle' n set walls (rock : rocks) jets board
            | key `Map.member` set = ((n, negate newHeight), fromJust $ key `Map.lookup` set)
            | otherwise            = findCycle' (n+1) set' walls rocks jets' board''
            where
                (placedRock, board') = placeRock walls (fst rock) board
                (jets', board'')     = simulateRock jets board' placedRock

                (newHeight, diffs, trimmed)  = trimTower walls board''

                key  = (snd rock, snd $ head jets', diffs)
                set' = Map.insert key (n, negate newHeight) set

trimTower :: (Int,Int) -> Map Coord Obj -> (Int, [Int], Map Coord Obj)
trimTower walls objs = (newHeight, diffs,) $ Map.filterWithKey (\(y,_) _ -> y <= lowerHeight) objs
    where
        newHeight     = negate $ rockHeight objs
        heightsPerCol = maxHeightPerCol walls objs newHeight
        lowerHeight   = maximum heightsPerCol

        diffs         = map (`subtract` newHeight) heightsPerCol

maxHeightPerCol :: (Int,Int) -> Map Coord Obj -> Int -> [Int]
maxHeightPerCol walls objs h = map (succ . maximum . map row . Set.toList) perCol
    where
        flooded   = flood Set.empty (h - 1, fst walls + 1)
        perCol    = map (\x -> Set.filter ((== x) . col) flooded) [fst walls + 1 .. snd walls - 1]

        valid flooded p   = p `Set.notMember` flooded &&  p `Map.notMember` objs

        flood flooded c
            | null neighs = flooded'
            | otherwise   = foldl flood flooded' neighs
            where
                flooded' = Set.insert c flooded
                neighs   = filter (valid flooded') [left c, down c, right c]

-- |
-- >>> :main
-- 3109
-- 1541449275365
main :: IO ()
main = do
    input <- init <$> readInputDefaults 2022 17

    let nRocks = 2022
        walls  = (0,8)
        
        -- Indexing the jets and rocks allows finding a cycle for part 2.
        fallingRocks = cycle $ zip rocks [1..]
        jets         = cycle $ zip input [1..]

        ground = Map.fromList $ [(( 0, x), Ground) | x <- [1..7]] ++
                                [((-y, 0), Wall)   | y <- [1..4]] ++
                                [((-y, 8), Wall)   | y <- [1..4]] ++
                                [(( 0, 0), Corner), (( 0, 8), Corner)]

        objects = simulate nRocks walls fallingRocks jets ground

    print $ rockHeight objects

    let targetRocks = 1000000000000
        
        ((r2, h2), (r1, h1)) = findCycle walls fallingRocks jets ground

        diffH = h2 - h1
        diffR = r2 - r1

        remRocks = targetRocks - r1

        (cycles, lastRocks) = remRocks `divMod` diffR

        objects'  = simulate (r1 + lastRocks) walls fallingRocks jets ground
        remHeight = rockHeight objects'

        totalHeight = cycles * diffH + remHeight

    print totalHeight
