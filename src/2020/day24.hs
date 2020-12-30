{-# Language OverloadedStrings #-}

import Advent.Life
import Advent.Megaparsec
import qualified Data.Set as Set

type AxialCoord  = (Int,Int)
type Tiles       = Set.Set AxialCoord
data Instruction = E | SE | SW | W | NW | NE

format :: Parser [Instruction]
format = manyTill instruction (lookAhead "\n")

instruction :: Parser Instruction
instruction = ("e"  $> E)
          <|> ("se" $> SE)
          <|> ("sw" $> SW)
          <|> ("w"  $> W)
          <|> ("nw" $> NW)
          <|> ("ne" $> NE)      

move :: AxialCoord -> Instruction -> AxialCoord
move (x,y) E  = (x - 1, y    )
move (x,y) SE = (x - 1, y + 1)
move (x,y) SW = (x    , y + 1)
move (x,y) W  = (x + 1, y    )
move (x,y) NW = (x + 1, y - 1)
move (x,y) NE = (x    , y - 1)

identify :: Tiles -> [Instruction] -> Tiles
identify tiles instructions
    | coords `Set.notMember` tiles = Set.insert coords tiles
    | otherwise                    = Set.delete coords tiles
    where coords = foldl move (0,0) instructions

neighbours :: AxialCoord -> [AxialCoord]
neighbours coord = map (move coord) [E, SE, SW, W, NW, NE]

rules :: Bool -> Int -> Bool
rules True  n = n == 1 || n == 2
rules False n = n == 2

main :: IO()
main = do
    instructions <- readParsedLines 2020 24 format

    let tiles    = foldl identify Set.empty instructions
        tiles100 = iterate (evolve neighbours rules) tiles !! 100

    print . countAlive $ tiles
    print . countAlive $ tiles100
