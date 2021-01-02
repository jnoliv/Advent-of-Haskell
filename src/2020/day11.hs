import AdventAPI
import Advent.Utils (readAsSet, count)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S

type SeatP = (Int,Int)

addCoord :: SeatP -> SeatP -> SeatP
addCoord (y,x) (y',x') = (y + y', x + x')

potentialNeighbours :: [SeatP]
potentialNeighbours = [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]

countAdjacentTakenSeats :: Set SeatP -> SeatP -> Int
countAdjacentTakenSeats taken p = count ((`S.member` taken) . addCoord p) potentialNeighbours

countVisibleTakenSeats :: Set SeatP -> (Int,Int) -> Set SeatP -> SeatP -> Int
countVisibleTakenSeats seats (ySize,xSize) taken p = length $ mapMaybe (visibleTakenSeats p) potentialNeighbours
    where visibleTakenSeats (y,x) d
            | y < 0 || ySize <= y    = Nothing
            | x < 0 || xSize <= x    = Nothing
            | p' `S.notMember` seats = visibleTakenSeats p' d
            | p' `S.member` taken    = Just p'
            | otherwise              = Nothing
            where p' = addCoord (y,x) d

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
