import qualified Common.AdventAPI as AdventAPI

import Common.Utils (count)
import Data.Maybe (catMaybes)
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
    where inds = map (`zip` [0..size]) . fmap (cycle . return) $ [0..size]
          seatToChar Taken = '#'
          seatToChar Empty = 'L'
          seatToChar Floor = '.'

-- | Apply the given rules to the given celular automaton until it stabilizes
stabilize :: (SeatMap -> Bool -> (Int, Int) -> Seat -> (Bool, Seat)) -> SeatMap -> SeatMap
stabilize rules seats = snd $ until (not . fst) stabilize' (True, seats)
    where stabilize' (_,seats') = M.mapAccumWithKey (rules seats') False seats'

-- Apply the celular automaton rules to the given position
applyRules :: SeatMap -> Bool -> (Int, Int) -> Seat -> (Bool, Seat)
applyRules seats acc pos s =
    let nAdj = countAdj seats pos
    in case s of
        Taken -> if nAdj >= 4 then (True, Empty) else (acc, s)
        Empty -> if nAdj == 0 then (True, Taken) else (acc, s)

-- | Count the number of taken adjacent seats
countAdj :: SeatMap -> (Int, Int) -> Int
countAdj seats (x,y) = count id . map ((==Taken) . flip (M.findWithDefault Empty) seats) $ allAdj
    where allAdj     = map (\(x',y') -> (x+x', y+y')) [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]

main :: IO()
main = do
    contents <- AdventAPI.readInputDefaults 11

    let seats  = readAsMap charToSeat contents
        stable = stabilize applyRules seats
        nTaken = M.foldr (\s a -> if s == Taken then a+1 else a) 0 stable :: Int

    print nTaken
