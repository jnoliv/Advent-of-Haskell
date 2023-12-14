{-# LANGUAGE ImportQualifiedPost #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid (col, down, left, right, row, up, Coord)
import Advent.Utils (readAsSet)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

tiltCycle :: (Int, Int) -> Set Coord -> Set Coord -> Set Coord
tiltCycle size flat round = foldl (flip apply) round [tiltN, tiltW, tiltS, tiltE]
    where
        apply f = f size flat

tiltN, tiltS, tiltW, tiltE :: (Int, Int) -> Set Coord -> Set Coord -> Set Coord
tiltN (rows, _) = tilt rows up    row          [0 .. rows - 1]
tiltS (rows, _) = tilt rows down  row (reverse [0 .. rows - 1])
tiltE (_, cols) = tilt cols right col (reverse [0 .. cols - 1])
tiltW (_, cols) = tilt cols left  col          [0 .. cols - 1]

tilt :: Int -> (Coord -> Coord) -> (Coord -> Int) -> [Int] -> Set Coord -> Set Coord -> Set Coord
tilt size dir get ns flat round = foldl (\acc n -> Set.union acc $ tiltLine acc n) Set.empty ns
    where
        tiltLine :: Set Coord -> Int -> Set Coord
        tiltLine round' n = Set.map roll roundInLine
            where
                roll        = until (not . canRoll round') dir
                roundInLine = Set.filter ((== n) . get) round

        canRoll :: Set Coord -> Coord -> Bool
        canRoll round' rock = inBounds && newRock `Set.notMember` flat && newRock `Set.notMember` round'
            where
                newRock  = dir rock
                inBounds = 0 <= get newRock && get newRock < size

load :: Int -> Set Coord -> Int
load rows round = sum $ map f [0 .. rows - 1]
    where
        f r = (rows - r) * Set.size (Set.filter ((==r) . row) round)

findFixedPoint :: (Int, Int) -> Set Coord -> Set Coord -> (Int, Int, Set Coord)
findFixedPoint size flat = f Map.empty 1
    where
        f visited n prev
            | cur `Map.member` visited = (visited Map.! cur, n, cur)
            | otherwise                = f (Map.insert cur n visited) (n + 1) cur
            where
                cur = tiltCycle size flat prev

-- |
-- >>> :main
-- 108935
-- 100876
main :: IO ()
main = do
    input <- readInputDefaults 2023 14

    let size  = (\ls -> (length ls, length (head ls))) $ lines input

        flat  = readAsSet (== '#') input
        round = readAsSet (== 'O') input

        cycles = 1000000000

        (n0, n1, round') = findFixedPoint size flat round
        remIters         = (cycles - n0) `rem` (n1 - n0)

        round'' = iterate (tiltCycle size flat) round' !! remIters

    print . load (fst size) $ tiltN size flat round
    print $ load (fst size) round''
