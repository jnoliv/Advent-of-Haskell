import Advent.API (readInputDefaults)
import Advent.Coord.Grid
import Data.Set (Set)
import qualified Data.Set as S

data Houses = Houses { santa :: Coord, robo :: Coord, next :: Bool, seen :: Set Coord}

dir :: Char -> Coord
dir '^' =    up (0,0)
dir '>' = right (0,0)
dir 'v' =  down (0,0)
dir '<' =  left (0,0)

move :: (Bool -> Bool) -> Houses -> Char -> Houses
move f (Houses s r  True seen) d = Houses s' r (f True)  (S.insert s' seen)
    where s' = s .+ dir d
move f (Houses s r False seen) d = Houses s r' (f False) (S.insert r' seen)
    where r' = r .+ dir d

-- |
-- >>> :main
-- 2592
-- 2360
main :: IO ()
main = do
    input <- readInputDefaults 2015 3

    let start   = Houses (0,0) (0,0) True (S.singleton (0,0))
        houses  = seen $ foldl (move id)  start input
        houses2 = seen $ foldl (move not) start input

    print . S.size $ houses
    print . S.size $ houses2
