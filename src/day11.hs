import qualified Common.AdventAPI as AdventAPI

import Common.Utils (count)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Map as M

data Seat = Taken | Empty | Floor
    deriving (Show, Eq)

type SeatMap = M.Map (Int,Int) Seat

-- | Read the ASCII input to a map. Receives a function to convert
-- from char to the map value, wrapped in Maybe to allow ommiting
-- positions from the resulting map
readAsMap :: (Char -> Maybe a) -> String -> M.Map (Int, Int) a
readAsMap f input = M.fromList . catMaybes . concatMap (map maybeT) $ zipWith zip inds ls
    where ls    = map (map f) . lines $ input
          inds  = map (`zip` [0..]) . fmap (cycle . return) $ [0..]
          maybeT (x, Just y)  = Just (x,y)
          maybeT (_, Nothing) = Nothing

-- | Convert an input character to a Maybe Seat
charToSeat :: Char -> Maybe Seat
charToSeat '#' = Just Taken
charToSeat 'L' = Just Empty
charToSeat  _ = Nothing

-- | Print a map, used for debugging
showMap :: SeatMap -> Int -> String
showMap m size = (++ "\n") . concatMap ((++ "\n") . map (seatToChar . flip (M.findWithDefault Floor) m)) $ inds
    where inds = map (`zip` [0..size -1]) . fmap (cycle . return) $ [0..size -1]
          seatToChar Taken = '#'
          seatToChar Empty = 'L'
          seatToChar Floor = '.'

-- | Apply the given rules to the given celular automaton until it stabilizes
stabilize :: (SeatMap -> Bool -> (Int, Int) -> Seat -> (Bool, Seat)) -> SeatMap -> SeatMap
stabilize rules seats = snd $ until (not . fst) stabilize' (True, seats)
    where stabilize' (_,seats') = M.mapAccumWithKey (rules seats') False seats'

-- Apply the part 1 celular automaton rules to the given position
applyRules1 :: SeatMap -> Bool -> (Int, Int) -> Seat -> (Bool, Seat)
applyRules1 seats acc pos s =
    let nAdj = countAdj1 seats pos
    in case s of
        Taken -> if nAdj >= 4 then (True, Empty) else (acc, s)
        Empty -> if nAdj == 0 then (True, Taken) else (acc, s)

-- | Count the number of taken adjacent seats (for part 1)
countAdj1 :: SeatMap -> (Int, Int) -> Int
countAdj1 seats (x,y) = count id . map ((==Taken) . flip (M.findWithDefault Empty) seats) $ allAdj
    where allAdj = map (\(x',y') -> (x+x', y+y')) [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]

-- Apply the part 2 celular automaton rules to the given position
applyRules2 :: (Int, Int) -> SeatMap -> Bool -> (Int, Int) -> Seat -> (Bool, Seat)
applyRules2 size seats acc pos s =
    let nAdj = countAdj2 seats size pos
    in case s of
        Taken -> if nAdj >= 5 then (True, Empty) else (acc, s)
        Empty -> if nAdj == 0 then (True, Taken) else (acc, s)

-- | Count the number of taken adjacent seats (for part 2)
countAdj2 :: SeatMap -> (Int,Int) -> (Int, Int) -> Int
countAdj2 seats size (x,y) = count id . map ((==Taken) . flip (M.findWithDefault Empty) seats) $ allAdj
    where allAdj = mapMaybe (findClosestSeat seats size (x,y)) [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]

findClosestSeat :: SeatMap -> (Int,Int) -> (Int,Int) -> (Int,Int) -> Maybe (Int,Int)
findClosestSeat seats size@(xSize,ySize) (x,y) d@(dx,dy)
    | x < 0 || xSize <= x                       = Nothing
    | y < 0 || ySize <= y                       = Nothing
    | M.findWithDefault Floor p seats /= Floor = Just p
    | otherwise                                = findClosestSeat seats size p d
    where p = (x + dx, y + dy)

-- | Count the number of taken seats
countTaken :: SeatMap -> Int
countTaken = M.foldr (\s a -> if s == Taken then a+1 else a) 0

main :: IO()
main = do
    contents <- AdventAPI.readInputDefaults 11

    let size  = (,) <$> length <*> length . head $ lines contents
        seats = readAsMap charToSeat contents

        stable1 = stabilize  applyRules1       seats
        stable2 = stabilize (applyRules2 size) seats

    print $ countTaken stable1
    print $ countTaken stable2
