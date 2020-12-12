import AdventAPI ( readInputDefaults )

import Data.List ( nub )

data Dir = N | E | S | W
    deriving (Show, Eq, Enum)

type Coord = (Int,Int)

-- | Given the current direction and a rotation tuple, returns the new direction
rotate :: Dir -> (Char, Int) -> Dir
rotate d ('R', a) = toEnum . (`mod` 4) . (+ (a `div` 90)) . fromEnum $ d
rotate d ('L', a) = toEnum . (`mod` 4) . (+ 4) . flip (-) (a `div` 90) . fromEnum $ d

-- | Convert a direction to coordinates delta
dirToDeltaTuple :: Dir -> Coord
dirToDeltaTuple N = (-1, 0)
dirToDeltaTuple E = (0, 1)
dirToDeltaTuple S = (1, 0)
dirToDeltaTuple W = (0, -1)

-- | Add coordinates
addCoord :: Coord -> Coord -> Coord
addCoord (x,y) (x',y') = (x + x', y + y')

-- | Multiply coordinates
multCoord :: Int -> Coord -> Coord
multCoord n (x,y) = (n * x, n * y)

-- | Navigate from starting direction and position using the list of instructions
navigate :: Dir -> Coord -> [(Char,Int)] -> Coord
navigate  _  pos []         = pos
navigate dir pos ((i,n):is) =
    case i of
        'N' -> navigate dir (addCoord pos . multCoord n $ dirToDeltaTuple N) is
        'E' -> navigate dir (addCoord pos . multCoord n $ dirToDeltaTuple E) is
        'S' -> navigate dir (addCoord pos . multCoord n $ dirToDeltaTuple S) is
        'W' -> navigate dir (addCoord pos . multCoord n $ dirToDeltaTuple W) is
        'L' -> navigate (rotate dir ('L',n)) pos is
        'R' -> navigate (rotate dir ('R',n)) pos is
        'F' -> navigate dir (addCoord pos . multCoord n $ dirToDeltaTuple dir) is
        e  -> error ("Unexpected instruction: " ++ [e])

parseInstructions :: String -> [(Char,Int)]
parseInstructions input = map (\s -> (head s, read $ tail s)) $ lines input

main :: IO()
main = do
    contents <- AdventAPI.readInputDefaults 12

    let instructions  = parseInstructions contents
        finalPos      = navigate E (0,0) instructions
        manhattanDist = (+) <$> abs . fst <*> abs . snd $ finalPos

    print manhattanDist
    