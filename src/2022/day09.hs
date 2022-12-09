{-# LANGUAGE ImportQualifiedPost, OverloadedStrings #-}

import Advent.API (readInputDefaults)
import Advent.Megaparsec
import Advent.Coord.Grid

import Data.Bifunctor

import Data.Set (Set)
import Data.Set qualified as Set

type Move  = (Coord -> Coord, Int)
type State = (Coord, [Coord], Set Coord) -- (Head, Tails, Visited by tail)

format :: Parser (Coord -> Coord, Int)
format = (,) <$> dir <* " " <*> decimal
    where
        dir = "L" $> left
          <|> "U" $> up
          <|> "R" $> right
          <|> "D" $> down

move :: Move -> State -> State
move (dir, n) = head . drop n . iterate (moveTails . moveHead dir)

moveHead :: (Coord -> Coord) -> (Coord, a, b) -> (Coord, a, b)
moveHead dir (head, tail, visited) = (dir head, tail, visited)

moveTails :: State -> State
moveTails s@(head, tails, visited) = (head, tails', Set.insert (last tails') visited)
    where
        tails' = moveTails' [] (head, tails)

        moveTails' :: [Coord] -> (Coord, [Coord]) -> [Coord]
        moveTails' newTails (_, []) = reverse newTails
        moveTails' newTails (head, (tail : tails)) = moveTails' (tail' : newTails) (tail', tails)
            where
                tail' = moveTail head tail

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

    let afterMovement  = foldl (flip move)  ((0,0), [(0,0)], Set.empty) moves

        tails          = take 9 $ repeat (0,0)
        afterMovement2 = foldl (flip move) ((0,0), tails, Set.empty) moves

    print . Set.size. (\(_, _, s) -> s) $ afterMovement
    print . Set.size. (\(_, _, s) -> s) $ afterMovement2
