import Advent.API
import Advent.Coord.Grid
import Advent.Utils (readAsSet, count)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S

countAdjacentTakenSeats :: Set Coord -> Coord -> Int
countAdjacentTakenSeats taken p = count (`S.member` taken) . neighbours8 $ p

countVisibleTakenSeats :: Set Coord -> (Int,Int) -> Set Coord -> Coord -> Int
countVisibleTakenSeats seats (ySize,xSize) taken p = length . mapMaybe (visibleTakenSeats p) . neighbours8 $ (0,0)
    where visibleTakenSeats (y,x) d
            | y < 0 || ySize <= y    = Nothing
            | x < 0 || xSize <= x    = Nothing
            | p' `S.notMember` seats = visibleTakenSeats p' d
            | p' `S.member` taken    = Just p'
            | otherwise              = Nothing
            where p' = (y,x) .+ d

rules :: Int -> Bool -> Int -> Bool
rules tolerance True  n = n < tolerance
rules _         False n = n == 0

evolve :: Ord a => Set a -> (Set a -> a -> Int) -> (Bool -> Int -> Bool) -> Set a -> Set a
evolve grid countLiveNeighbours rules alive = S.filter filterF grid
    where filterF p = rules (p `S.member` alive) (countLiveNeighbours alive p)

stabilize :: Ord a => (Set a -> Set a) -> Set a -> Set a
stabilize evolve initial = stabilize' initial (evolve initial)
    where stabilize' st0 st1
            | st0 == st1 = st0
            | otherwise  = stabilize' st1 (evolve st1)

-- |
-- >>> :main
-- 2113
-- 1865
main :: IO()
main = do
    contents <- readInputDefaults 2020 11

    let size  = (,) <$> length <*> length . head $ lines contents
        seats = readAsSet (== 'L') contents

        stable1 = stabilize (evolve seats countAdjacentTakenSeats             (rules 4)) seats
        stable2 = stabilize (evolve seats (countVisibleTakenSeats seats size) (rules 5)) seats

    print . S.size $ stable1
    print . S.size $ stable2
