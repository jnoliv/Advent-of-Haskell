module Advent.Coord.Grid (
    Coord, (.+), (.-), (.*), (*.),
    up, right, down, left,
    turnLeft, turnRight, turnAround,
    distance, manhattan, neighbours4, neighbours8
) where

import Data.Bifunctor (bimap)

type Coord = (Int,Int) -- (row, col)

-- | Element access
row, col :: Coord -> Int
row = fst
col = snd

-- | Vector addition
(.+) :: Coord -> Coord -> Coord
(x,y) .+ (z,w) = (x + z, y + w)

-- | Vector subtraction
(.-) ::  Coord -> Coord -> Coord
(x,y) .- (z,w) = (x - z, y - w)

-- | Vector scaling, scalar on the left
(.*) :: Int -> Coord -> Coord
n .* (x,y) = (n * x, n * y)

-- | Vector scaling, scalar on the right
(*.) :: Coord -> Int -> Coord
(x,y) *. n = (n * x, n * y)

-- | Directional unitary movement
up, right, down, left :: Coord -> Coord
up    = (.+) (-1, 0)
right = (.+) ( 0, 1)
down  = (.+) ( 1, 0)
left  = (.+) ( 0,-1)

-- | Rotations around origin
turnLeft, turnRight, turnAround :: Coord -> Coord
turnLeft   (r,c) = (-c,  r)
turnRight  (r,c) = ( c, -r)
turnAround (r,c) = (-r, -c)

-- | Distance without diagonal movement
distance :: Coord -> Coord -> Int
distance p = manhattan . (.-) p

-- | Manhattan distance to origin
manhattan :: Coord -> Int
manhattan = uncurry (+) . bimap abs abs

-- | Four immediate neighbours, excluding diagonally adjacent
neighbours4 :: Coord -> [Coord]
neighbours4 = sequence [up, right, down, left]

-- | Eight immediate neighbours, including diagonally adjacent
neighbours8 :: Coord -> [Coord]
neighbours8 = sequence [up, right, down, left, (.+) (-1,-1), (.+) (-1,1), (.+) (1,-1), (.+) (1,1)]
