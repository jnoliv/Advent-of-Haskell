{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec (Parser, readParsedLines, decimal, (<|>), ($>))
import Advent.Coord.Grid (Coord, origin, left, up, right, down, (.+), (.-))
import Data.Bifunctor (bimap)

import Data.Set (Set)
import Data.Set qualified as Set

type Move  = (Coord -> Coord, Int)
type State = ([Coord], Set Coord)

format :: Parser (Coord -> Coord, Int)
format = (,) <$> dir <* " " <*> decimal
    where
        dir = "L" $> left
          <|> "U" $> up
          <|> "R" $> right
          <|> "D" $> down

move :: Move -> State -> State
move (dir, n) = head . drop n . iterate (moveTails . moveHead dir)

moveHead :: (Coord -> Coord) -> ([Coord], Set Coord) -> ([Coord], Set Coord)
moveHead dir (head : tail, visited) = (dir head : tail, visited)

moveTails :: State -> State
moveTails (knots, visited) = (knots', Set.insert (last knots') visited)
    where
        knots' = scanl1 moveTail knots

moveTail :: Coord -> Coord -> Coord
moveTail head tail =
    if tooFar diff
        then tail .+ (bimap signum signum diff)
        else tail
    where
        diff         = head .- tail
        tooFar (y,x) = (y < -1 || 1 < y) || (x < -1 || 1 < x)

-- |
-- >>> :main
-- 6376
-- 2607
main :: IO ()
main = do
    moves <- readParsedLines 2022 9 format

    let afterMovement  = foldl (flip move) (take  2 $ repeat origin, Set.empty) moves
        afterMovement2 = foldl (flip move) (take 10 $ repeat origin, Set.empty) moves

    print . Set.size . snd $ afterMovement
    print . Set.size . snd $ afterMovement2
