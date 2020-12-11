import AdventAPI
import Data.Maybe (mapMaybe)
import Utils (readAsMap, count)
import qualified Data.Map as M

data Seat = Taken | Empty | Floor
    deriving (Show, Eq)

type SeatMap = M.Map (Int,Int) Seat

type MakeNeigh = SeatMap -> (Int,Int) -> (Int,Int) -> (Int,Int) -> Maybe (Int,Int)

-- | Convert an input character to a Maybe Seat
charToSeat :: Char -> Maybe Seat
charToSeat '#' = Just Taken
charToSeat 'L' = Just Empty
charToSeat  _ = Nothing

-- | Apply the given rules to the given celular automaton until it stabilizes
stabilize :: (SeatMap -> Bool -> (Int, Int) -> Seat -> (Bool, Seat)) -> SeatMap -> SeatMap
stabilize rules seats = snd $ until (not . fst) stabilize' (True, seats)
    where stabilize' (_,seats') = M.mapAccumWithKey (rules seats') False seats'

-- Apply the rules to the given position
applyRules :: Int -> MakeNeigh -> (Int,Int) -> SeatMap -> Bool -> (Int,Int) -> Seat -> (Bool,Seat)
applyRules tolerance f size seats acc pos s =
    case s of
        Taken -> if nAdj >= tolerance then (True, Empty) else (acc, s)
        Empty -> if nAdj == 0         then (True, Taken) else (acc, s)
    where
        allAdj = mapMaybe (f seats size pos) [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]
        nAdj   = count id . map ((==Taken) . flip (M.findWithDefault Empty) seats) $ allAdj

-- | Immediately adjacent seats
adjacentSeat :: MakeNeigh
adjacentSeat _ _ (x,y) (dx,dy) = Just (x + dx, y + dy)

-- | Line of sight seats
visibleSeat :: MakeNeigh
visibleSeat seats size@(xSize,ySize) (x,y) d@(dx,dy)
    | x < 0 || xSize <= x                      = Nothing
    | y < 0 || ySize <= y                      = Nothing
    | M.findWithDefault Floor p seats /= Floor = Just p
    | otherwise                                = visibleSeat seats size p d
    where p = (x + dx, y + dy)

-- | Count the number of taken seats
countTaken :: SeatMap -> Int
countTaken = M.foldr (\s a -> if s == Taken then a+1 else a) 0

main :: IO()
main = do
    contents <- AdventAPI.readInputDefaults 11

    let size  = (,) <$> length <*> length . head $ lines contents
        seats = readAsMap charToSeat contents

        stable1 = stabilize (applyRules 4 adjacentSeat size) seats
        stable2 = stabilize (applyRules 5 visibleSeat  size) seats

    print $ countTaken stable1
    print $ countTaken stable2
