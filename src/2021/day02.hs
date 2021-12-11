{-# LANGUAGE OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Coord.Grid (Coord, (.+), (.*), up, right, down)
import Advent.Megaparsec (letterChar, decimal, many, Parser, readParsedLines)

format :: Parser (String, Int)
format = (,) <$> many letterChar <*> (" " *> decimal)

-- |
-- >>> :main
-- 2091984
-- 2086261056
main :: IO ()
main = do
    input <- readParsedLines 2021 2 format

    print . uncurry (*)       $ foldl pilot        (0,0)     input
    print . uncurry (*) . fst $ foldl pilotWithAim ((0,0),0) input

-- | Translates the coordinates in a direction and step
-- defined by the given tuple.
-- >>> pilot (0,2) ("up", 3)
-- (-3,2)
--
-- >>> pilot (1,-3) ("down", 7)
-- (8,-3)
--
-- >>> pilot (-1,2) ("forward", 5)
-- (-1,7)
pilot :: Coord -> (String, Int) -> Coord
pilot pos ("up",      step) = pos .+ (step .* up    (0,0)) 
pilot pos ("down",    step) = pos .+ (step .* down  (0,0)) 
pilot pos ("forward", step) = pos .+ (step .* right (0,0))

-- | Depending on the command, either adjusts the current aim,
-- or moves based on the step and the current aim.
-- >>> pilotWithAim ((4,1),7) ("up",3)
-- ((4,1),4)
--
-- >>> pilotWithAim ((-2,5),7) ("down",3)
-- ((-2,5),10)
--
-- >>> pilotWithAim ((7,2),4) ("forward",3)
-- ((19,5),4)
pilotWithAim :: (Coord, Int) -> (String, Int) -> (Coord, Int)
pilotWithAim (pos,   aim) ("up",      step) = (pos, aim - step)
pilotWithAim (pos,   aim) ("down",    step) = (pos, aim + step)
pilotWithAim ((y,x), aim) ("forward", step) = ((y + aim * step, x + step), aim)
