{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.Megaparsec
import Advent.Life
import Data.List
import Data.Set (Set)
import Data.Set qualified as Set

import Debug.Trace

type Range = (Int,Int)
data Step  = S Bool Range Range Range
    deriving Show

type Cube = (Int,Int,Int)

-- on x=10..12,y=10..12,z=10..12
format :: Parser Step
format = S <$> cmd
           <*> (" x=" *> range)
           <*> (",y=" *> range)
           <*> (",z=" *> range)
    where
        cmd   = try (("on" $> True)
                <|> ("off" $> False))
        range = (,) <$> sdecimal <* ".." <*> sdecimal

-- |
-- >>> :main
-- 
main :: IO ()
main = do
    input <- readParsedLines 2021 22 format

    let (initSteps, _) = partition inInitArea input

        initialized = initProcedure initSteps

    print . Set.size $ initProcedure initSteps

inInitArea :: Step -> Bool
inInitArea (S _ x y z) = f x && f y && f z
    where
        f (lower, upper) = (-50) <= lower && upper <= 50

inArea :: Range -> Int -> Bool
inArea (lower, upper) v = lower <= v && v <= upper

initProcedure :: [Step] -> Set Cube
initProcedure = foldl f Set.empty
    where
        range (l,u) = [l..u]

        f s (S False rx ry rz) = Set.filter (\(x,y,z) -> not (inArea rx x && inArea ry y && inArea rz z)) s
        f s (S True  rx ry rz) = Set.union s . Set.fromList $
            [(x,y,z) | x <- range rx, y <- range ry, z <- range rz]
