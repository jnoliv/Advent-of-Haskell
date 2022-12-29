{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec (Parser, readParsedLines, trim, decimal)
import Advent.Coord.Grid3 (Point, getX, getY, getZ, neighbours6)
import Advent.Life
import Advent.Math
import Advent.Utils

import Data.Set (Set)
import Data.Set qualified as Set

format :: Parser Point
format = (,,) <$> decimal <*> trim "," decimal "," <*> decimal

countExposed :: Set Point -> Int -> Point -> Int
countExposed set n p = n + count (`Set.notMember` set) (neighbours6 p)

countExteriorSurface :: Set Point -> [Point] -> Int
countExteriorSurface set points = 
    foldl (\n p -> n + count (`Set.member` flooded) (neighbours6 p)) 0 points
    where
        flooded = flood Set.empty (minX, minY, minZ)

        [minX, minY, minZ] = map (\f -> pred . minimum $ map f points) [getX, getY, getZ]
        [maxX, maxY, maxZ] = map (\f -> succ . maximum $ map f points) [getX, getY, getZ]

        insideFloodingArea (x,y,z) = between x minX maxX && between y minY maxY && between z minZ maxZ
            where
                between b a c = a <= b && b <= c
        
        valid flooded p = insideFloodingArea p && p `Set.notMember` flooded && p `Set.notMember` set

        flood :: Set Point -> Point -> Set Point
        flood flooded c = foldl flood flooded' neighs
            where
                flooded' = c `Set.insert` flooded
                neighs   = filter (valid flooded') $ neighbours6 c

-- |
-- >>> :main
-- 3390
-- 2058
main :: IO ()
main = do
    points <- readParsedLines 2022 18 format

    let set       = Set.fromList points
    
        nExposed  = foldl (countExposed set) 0 points
        nExposed' = countExteriorSurface set points

    print $ nExposed
    print $ nExposed'
