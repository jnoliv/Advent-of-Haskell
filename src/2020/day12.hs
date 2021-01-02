import AdventAPI (readInputDefaults)

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

-- | Manhattan distance from origin
manhattanDist :: Coord -> Int
manhattanDist = (+) <$> abs . fst <*> abs . snd

-- | Navigate from starting direction and position using the list of instructions
navigateShip :: Dir -> Coord -> [(Char,Int)] -> Coord
navigateShip  _  pos []         = pos
navigateShip dir pos ((i,n):is) =
    case i of
        'N' -> navigateShip dir (addCoord pos . multCoord n $ dirToDeltaTuple N) is
        'E' -> navigateShip dir (addCoord pos . multCoord n $ dirToDeltaTuple E) is
        'S' -> navigateShip dir (addCoord pos . multCoord n $ dirToDeltaTuple S) is
        'W' -> navigateShip dir (addCoord pos . multCoord n $ dirToDeltaTuple W) is
        'L' -> navigateShip (rotate dir ('L',n)) pos is
        'R' -> navigateShip (rotate dir ('R',n)) pos is
        'F' -> navigateShip dir (addCoord pos . multCoord n $ dirToDeltaTuple dir) is
        e   -> error ("Unexpected instruction: " ++ [e])

-- | Navigate from starting direction and position using the list of instructions
navigateWaypoint :: Coord -> Coord -> [(Char,Int)] -> Coord
navigateWaypoint shipPos   _    []         = shipPos
navigateWaypoint shipPos wayPos ((i,n):is) =
    case i of
        'N' -> navigateWaypoint shipPos (addCoord wayPos . multCoord n $ dirToDeltaTuple N) is
        'E' -> navigateWaypoint shipPos (addCoord wayPos . multCoord n $ dirToDeltaTuple E) is
        'S' -> navigateWaypoint shipPos (addCoord wayPos . multCoord n $ dirToDeltaTuple S) is
        'W' -> navigateWaypoint shipPos (addCoord wayPos . multCoord n $ dirToDeltaTuple W) is
        'L' -> navigateWaypoint shipPos (rotateWaypoint wayPos ('L',n)) is
        'R' -> navigateWaypoint shipPos (rotateWaypoint wayPos ('R',n)) is
        'F' -> navigateWaypoint (addCoord shipPos $ multCoord n wayPos) wayPos is
        e   -> error ("Unexpected instruction: " ++ [e])

-- | Rotate the waypoint around the ship (origin) the given amount in the given direction
rotateWaypoint :: Coord -> (Char,Int) -> Coord
rotateWaypoint (x,y) (d,n)
    | (d == 'L' && n == 90) || (d == 'R' && n == 270) = (-y,  x)
    | (d == 'L' && n == 270) || (d == 'R' && n == 90) = ( y, -x)
    | n == 180                                        = (-x, -y)

parseInstructions :: String -> [(Char,Int)]
parseInstructions input = map (\s -> (head s, read $ tail s)) $ lines input

-- |
-- >>> :main
-- 590
-- 42013
main :: IO()
main = do
    contents <- readInputDefaults 2020 12

    let instructions  = parseInstructions contents
        finalPos1     = navigateShip E (0,0) instructions
        distance1     = manhattanDist finalPos1

        finalPos2     = navigateWaypoint (0,0) (-1,10) instructions
        distance2     = manhattanDist finalPos2

    print distance1
    print distance2
    