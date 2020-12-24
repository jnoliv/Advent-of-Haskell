{-# Language OverloadedStrings #-}

import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Data.Set as Set
import Text.Megaparsec (manyTill, lookAhead)
import Utils (Parser, readParsedLines, count)

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

countBlackNeighbours :: Tiles -> AxialCoord -> Int
countBlackNeighbours tiles = count (`Set.member` tiles) . neighbours

keepBlack :: Int -> Bool
keepBlack n = n == 1 || n == 2

whiteNeighbours :: Tiles -> Tiles
whiteNeighbours set = Set.filter (`Set.notMember` set) allNeighs
    where allNeighs = Set.fromList . concatMap neighbours . Set.toList $ set

flipAll :: Tiles -> Tiles
flipAll set = keep `Set.union` new
    where keep = Set.filter (keepBlack . countBlackNeighbours set) set
          new  = Set.filter ((==2) . countBlackNeighbours set) (whiteNeighbours set)

main :: IO()
main = do
    instructions <- readParsedLines 24 format

    let tiles    = foldl identify Set.empty instructions
        tiles100 = iterate flipAll tiles !! 100

    print . Set.size $ tiles
    print . Set.size $ tiles100
