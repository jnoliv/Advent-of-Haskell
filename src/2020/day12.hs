import Advent.API (readInputDefaults)
import Advent.Coord.Grid
import Data.Bifunctor (first, second)

data Mode = V1 | V2

rotate :: Char -> Int -> Coord -> Coord
rotate 'L'  90 =   turnLeft
rotate 'R' 270 =   turnLeft
rotate 'L' 270 =  turnRight
rotate 'R'  90 =  turnRight
rotate  _  180 = turnAround

navigate :: Mode -> Coord -> Coord -> [(Char,Int)] -> Coord
navigate V1 pos wayP = snd . foldl (move V1) (pos, wayP)
navigate V2 pos wayP = fst . foldl (move V2) (pos, wayP)

move :: Mode -> (Coord, Coord) -> (Char,Int) -> (Coord, Coord)
move _  p ('N',n) = second (.+ (-n, 0)) p
move _  p ('E',n) = second (.+ ( 0, n)) p
move _  p ('S',n) = second (.+ ( n, 0)) p
move _  p ('W',n) = second (.+ ( 0,-n)) p

move V1 p ('L',n) = first  (rotate 'L' n) p
move V2 p ('L',n) = second (rotate 'L' n) p
move V1 p ('R',n) = first  (rotate 'R' n) p
move V2 p ('R',n) = second (rotate 'R' n) p

move V1 (pos, wayP) ('F',n) = (pos, (wayP .+ (n .* pos)))
move V2 (pos, wayP) ('F',n) = ((pos .+ (n .* wayP)), wayP)

parseInstructions :: String -> [(Char,Int)]
parseInstructions = map ((,) <$> head <*> read . tail) . lines

-- |
-- >>> :main
-- 590
-- 42013
main :: IO()
main = do
    instructions <- parseInstructions <$> readInputDefaults 2020 12

    let finalPos1 = navigate V1 (0,1) ( 0, 0) instructions
        finalPos2 = navigate V2 (0,0) (-1,10) instructions

    print . manhattan $ finalPos1
    print . manhattan $ finalPos2
    