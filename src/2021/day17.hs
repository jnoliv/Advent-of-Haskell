{-# LANGUAGE OverloadedStrings #-}

import Advent.Megaparsec (Parser, readParsed, sdecimal)
import Data.Maybe (mapMaybe)

data Probe = P (Int,Int) (Int,Int) -- P (y,x) (dy,dx)
data Range = R (Int,Int) (Int,Int) -- P (min x, max x) (min y, max y)

format :: Parser Range
format = R <$> ("target area: x=" *> range <* ", y=") <*> range
    where
        range = (,) <$> sdecimal <* ".." <*> sdecimal

-- |
-- >> :main
-- 
main :: IO ()
main = do
    target <- readParsed 2021 17 format

    let R (xl, xu) (yl, yu) = target

        starts = [P (0,0) (y,x) | y <- [-1000..1000], x <- [-1000..1000]]
        ys     = mapMaybe (sim target) starts

    print $ maximum ys
    print $ length ys

sim :: Range -> Probe -> Maybe Int
sim range probe = f probe 0
    where
        f probe maxY
            | onTarget range probe = Just maxY
            | overshot range probe = Nothing
            | otherwise            = f probe' (max maxY y)
                where
                    probe'@(P (y,_) _) = step probe

step :: Probe -> Probe
step (P (y,x) (dy, dx)) = P (y + dy,x + dx) (dy - 1, drag dx)
    where
        drag dx
            | dx > 0    = dx - 1
            | dx < 0    = dx + 1
            | otherwise = 0

-- |
-- >>> onTarget (R (20,30) (-10,-5)) (P (-7,28) (-5,0))
-- True
--
-- >>> onTarget (R (20,30) (-10,-5)) (P (2,22) (-2,3))
-- False
onTarget :: Range -> Probe -> Bool
onTarget (R xr yr) (P (y,x) _) = f yr y && f xr x
    where
        f (n, n') v = n <= v && v <= n'

overshot :: Range -> Probe -> Bool
overshot (R (_,xu) (yl,_)) (P (y,x) _) = y < yl || x > xu
