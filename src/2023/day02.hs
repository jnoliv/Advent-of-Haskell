{-# LANGUAGE OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec (decimal, sepBy1, (<|>), Parser, readParsedLines, trim)

data Draw = D Int Int Int -- R G B
type Game = (Int, [Draw])

setR r (D _ g b) = D r g b
setG g (D r _ b) = D r g b
setB b (D r g _) = D r g b

format :: Parser Game
format = (,) <$> parseId
             <*> parseDraw `sepBy1` "; "
    where
        parseId    = trim "Game " decimal ": "
        parseDraw  = foldr ($) (D 0 0 0)
                        <$> parseCubes `sepBy1` ", "
        parseCubes = flip ($) <$> decimal
                              <*  " "
                              <*> parseCube
        parseCube  = (setR <$ "red")
                 <|> (setG <$ "green")
                 <|> (setB <$ "blue")

isPossible :: (Int, Int, Int) -> Game -> Bool
isPossible (r, g, b) = all pred . snd
    where
        pred (D r' g' b') = (r' <= r) && (g' <= g) && (b' <= b)

minDraw :: Game -> Draw
minDraw = foldr1 minCubes . snd
    where
        minCubes (D r1 g1 b1) (D r2 g2 b2) = D (max r1 r2) (max g1 g2) (max b1 b2)

power :: Draw -> Int
power (D r g b) = r * g * b

-- |
-- >>> :main
-- 2716
-- 72227
main :: IO ()
main = do
    input <- readParsedLines 2023 2 format

    let cubes = (12, 13, 14)

    print . sum . map fst . filter (isPossible cubes) $ input

    print . sum . map (power . minDraw) $ input
