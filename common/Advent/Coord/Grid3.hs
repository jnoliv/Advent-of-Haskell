{-# LANGUAGE ImportQualifiedPost #-}

module Advent.Coord.Grid3 (
    Point,
    (.+), (.-),
    manhattan,
    orientations,
) where

import Data.List (transpose)
import Data.Set (Set)
import Data.Set qualified as Set

type Point  = (Int,Int,Int) -- x y z
type Matrix = [[Int]]

(.-) :: Point -> Point -> Point
(x1,y1,z1) .- (x2,y2,z2) = (x1 - x2, y1 - y2, z1 - z2)

(.+) :: Point -> Point -> Point
(x1,y1,z1) .+ (x2,y2,z2) = (x1 + x2, y1 + y2, z1 + z2)

manhattan :: Point -> Point -> Int
manhattan (x1,y1,z1) (x2,y2,z2) = sum $ map abs [x1 - x2, y1 - y2, z1 - z2]

orientations :: Set Point -> [Set Point]
orientations set = map (\m -> Set.map (rotate m) set) orientationMatrices
    where
        rotate m (x,y,z) = (\[[a,b,c]] -> (a,b,c)) $ mmult [[x,y,z]] m

mmult :: Matrix -> Matrix -> Matrix
mmult a b = [[sum $ zipWith (*) rowsA colsB | colsB <- transpose b] | rowsA <- a]

-- | Matrices taken from
-- http://www.euclideanspace.com/maths/algebra/matrix/transforms/examples/index.htm
orientationMatrices :: [Matrix]
orientationMatrices = [
        [[1,0,0],[0,1,0],[0,0,1]],
        [[1,0,0],[0,0,-1],[0,1,0]],
        [[1,0,0],[0,-1,0],[0,0,-1]],
        [[1,0,0],[0,0,1],[0,-1,0]],

        [[0,-1,0],[1,0,0],[0,0,1]],
        [[0,0,1],[1,0,0],[0,1,0]],
        [[0,1,0],[1,0,0],[0,0,-1]],
        [[0,0,-1],[1,0,0],[0,-1,0]],

        [[-1,0,0],[0,-1,0],[0,0,1]],
        [[-1,0,0],[0,0,-1],[0,-1,0]],
        [[-1,0,0],[0,1,0],[0,0,-1]],
        [[-1,0,0],[0,0,1],[0,1,0]],

        [[0,1,0],[-1,0,0],[0,0,1]],
        [[0,0,1],[-1,0,0],[0,-1,0]],
        [[0,-1,0],[-1,0,0],[0,0,-1]],
        [[0,0,-1],[-1,0,0],[0,1,0]],

        [[0,0,-1],[0,1,0],[1,0,0]],
        [[0,1,0],[0,0,1],[1,0,0]],
        [[0,0,1],[0,-1,0],[1,0,0]],
        [[0,-1,0],[0,0,-1],[1,0,0]],

        [[0,0,-1],[0,-1,0],[-1,0,0]],
        [[0,-1,0],[0,0,1],[-1,0,0]],
        [[0,0,1],[0,1,0],[-1,0,0]],
        [[0,1,0],[0,0,-1],[-1,0,0]]
    ]
