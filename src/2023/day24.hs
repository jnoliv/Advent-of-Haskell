{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec (readParsedLines, sdecimal, sepBy, Parser)
import Advent.Coord.Grid3 (getX, getY, Point)
import Advent.Utils (combinations)

import Data.Maybe (mapMaybe)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import GHC.Float (int2Double)

type Vector = Point
type Line   = (Point,Vector)

type PointF = (Double,Double,Double)

parser :: Parser Line
parser = (,) <$> triple <* " @ " <*> triple
    where
        triple = (\[a,b,c] -> (a,b,c)) <$> sdecimal `sepBy` ", "

add :: Point -> PointF -> PointF
add (x0,y0,z0) (x1,y1,z1) = (
        int2Double x0 + x1,
        int2Double y0 + y1,
        int2Double z0 + z1
   )

scale :: Double -> Vector -> PointF
scale f (x,y,z) = (f * int2Double x, f * int2Double y, f * int2Double z)

-- https://stackoverflow.com/a/2932601
intersection2D :: Line -> Line -> Maybe PointF
intersection2D (as,ad) (bs,bd)
    | det == 0         = Nothing
    | u >= 0 && v >= 0 = Just (as `add` (u `scale` ad))
    | otherwise        = Nothing
    where
        dx = getX bs - getX as
        dy = getY bs - getY as

        det = getX bd * getY ad - getY bd * getX ad

        u = int2Double (dy * getX bd - dx * getY bd) / int2Double det
        v = int2Double (dy * getX ad - dx * getY ad) / int2Double det

inside :: Double -> Double -> PointF -> Bool
inside s e (x,y,_) = s <= x && x <= e && s <= y && y <= e

-- |
-- >>> :main
-- 13149
-- 1033770143421619
main :: IO ()
main = do
    input <- readParsedLines 2023 24 parser

    let pairs = map (\[l1,l2] -> (l1,l2)) $ combinations 2 input

        min = 200000000000000
        max = 400000000000000

    print . length $ filter (inside min max) $ mapMaybe (uncurry intersection2D) pairs

{-
Part 2 solved using a free trial of Mathematica Cloud.

Each hailstone trajectory can be described as Q + t U, where Q is the starting point and U the velocity vector.
The thrown rock intersects with a hailstone at time t, if
    P + t V = Q + t U
where P is the rock's starting point and V its velocity vector.

Thus, we want to solve, for all hailstones i,
    P + t[i] V = Q[i] + t[i] U[i]
which can be broken up into identical equations for each coordinate:
    px + t[i] vx = Q[i].x + t[i] U[i].x
    py + t[i] vy = Q[i].y + t[i] U[i].y
    pz + t[i] vz = Q[i].z + t[i] U[i].z

Since the set of equations contains 6 variables (px,py,pz,vx,vy,vz) and an additional one per hailstone (t[i]), using
the first 3 hailstones of the input is sufficient to get a set of 9 equations with 9 variables. Using my actual input,
these are:
    x + t0 * vx == 225004689740965 + 275 * t0
    y + t0 * vy == 150875733412640 + 389 * t0
    z + t0 * vz == 116049940893518 + 375 * t0
    x + t1 * vx == 338282582546422 - 162 * t1
    y + t1 * vy == 191340608518886 +  84 * t1
    z + t1 * vz == 340003210160681 -  46 * t1
    x + t2 * vx == 276063330011297 -   9 * t2
    y + t2 * vy == 506267063607948 - 360 * t2
    z + t2 * vz == 451688278442130 - 275 * t2

Inputting these into Mathematica Cloud, like so:
    Solve[
        x + t0 * vx == 225004689740965 + 275 * t0 &&
        y + t0 * vy == 150875733412640 + 389 * t0 &&
        z + t0 * vz == 116049940893518 + 375 * t0 &&
        x + t1 * vx == 338282582546422 - 162 * t1 &&
        y + t1 * vy == 191340608518886 +  84 * t1 &&
        z + t1 * vz == 340003210160681 -  46 * t1 &&
        x + t2 * vx == 276063330011297 -   9 * t2 &&
        y + t2 * vy == 506267063607948 - 360 * t2 &&
        z + t2 * vz == 451688278442130 - 275 * t2,

        {x,y,z,vx,vy,vz,t0,t1,t2}, Integers
    ]

yields the following result:
    {{
        x->454198524200037,
        y->345331129547776,
        z->234240489673806,
        vx->-299,
        vy->-98,
        vz->79,
        t0->399292394528,
        t1->846101763895,
        t2->614259290306
    }}

The answer is x + y + z, i.e., 1033770143421619.
-}
