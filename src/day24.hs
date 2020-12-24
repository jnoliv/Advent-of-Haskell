{-# Language OverloadedStrings #-}

import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Data.Set as Set
import Text.Megaparsec (manyTill, lookAhead)
import Utils (Parser, readParsedLines)

type AxialCoord  = (Int,Int)
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

move :: Instruction -> AxialCoord -> AxialCoord
move E  (x,y) = (x - 1, y    )
move SE (x,y) = (x - 1, y + 1)
move SW (x,y) = (x    , y + 1)
move W  (x,y) = (x + 1, y    )
move NW (x,y) = (x + 1, y - 1)
move NE (x,y) = (x    , y - 1)

identify :: Set.Set AxialCoord -> [Instruction] -> Set.Set AxialCoord
identify tiles instructions
    | coords `Set.notMember` tiles = Set.insert coords tiles
    | otherwise                    = Set.delete coords tiles
    where coords = foldl (flip move) (0,0) instructions

main :: IO()
main = do
    instructions <- readParsedLines 24 format

    let tiles = foldl identify Set.empty instructions

    print . Set.size $ tiles
